import streams
import sets
import options
import sequtils
import strutils
import strformat

const IdentStartChars =
  strutils.IdentStartChars + {'+', '-', '*', '/', ';', '>', '<', '=', '!', '$', '%', '^', '?', '.', '\''}
const IdentChars = IdentStartChars + Digits

type 
  ParseOptions* = object
    ## A list of characters to force the parser to split on. For example, if
    ## '.' is present in this list, then `foo.bar` will become 3 separate
    ## symbols, `foo`, `.`, `bar` - whereas without '.' being present, this
    ## will be parsed as a single symbol - `foo.bar`.
    forceSplitChars: HashSet[char]
  SexprKind* = enum
    skList
    skInt
    skFloat
    skString
    skSym
  Sexpr* = object
    line*: int
    col*: int
    case kind*: SexprKind
    of skList: listVal*: seq[Sexpr]
    of skInt: intVal*: int64
    of skFloat: floatVal*: float64
    of skString: stringVal*: string
    of skSym: symVal*: string

proc `==`*(a: Sexpr, b: Sexpr): bool =
  if a.kind != b.kind: return false
  case a.kind
  of skInt: a.intVal == b.intVal
  of skFloat: a.floatVal == b.floatVal
  of skString: a.stringVal == b.stringVal
  of skSym: a.symVal == b.symVal
  of skList: a.listVal == b.listVal

type UnbalancedParensError = object of CatchableError
type UnexpectedCharacterError = object of CatchableError

proc initParseOptions*(): ParseOptions = ParseOptions(forceSplitChars: initHashSet[char]())
proc withForceSplitChar*(p: ParseOptions, c: char): ParseOptions =
  var p = p
  p.forceSplitChars.incl(c)
  p

proc parse*(input: Stream, opt: ParseOptions): Option[Sexpr]

proc consumeWhitespace(input: Stream, opt: ParseOptions) =
  while input.peekChar() in Whitespace:
    let _ = input.readChar()

proc parseNumber(input: Stream, opt: ParseOptions): Sexpr =
  var buf = ""
  var numPoints = 0
  type NumTy = enum
    ntDec
    ntHex
    ntBin
  var numTy = ntDec
  var prefix : array[2, char]
  try:
    input.peek(prefix)
    if prefix == ['0', 'x']:
      numTy = ntHex
      input.setPosition(input.getPosition + 2)
    if prefix == ['0', 'b']:
      numTy = ntBin
      input.setPosition(input.getPosition + 2)
  except IOError: discard

  while true:
    let currChar = input.peekChar
    case currChar
    of Digits: buf.add(currChar)
    of {'a'..'f', 'A'..'F'}:
      if numTy == ntHex: buf.add(currChar)
      else: break
    of '.':
      if numPoints < 1:
        buf.add(currChar)
      numPoints += 1
    else: break
    let _ = input.readChar
  if numPoints > 0: return Sexpr(line: 0, col: 0, kind: skFloat, floatVal: buf.parseFloat)
  elif numTy == ntHex:
    return Sexpr(line: 0, col: 0, kind: skInt, intVal: buf.parseHexInt)
  elif numTy == ntBin:
    return Sexpr(line: 0, col: 0, kind: skInt, intVal: buf.parseBinInt)
  else: return Sexpr(line: 0, col: 0, kind: skInt, intVal: buf.parseInt)

proc parseList(input: Stream, opt: ParseOptions): Sexpr =
  assert input.readChar() == '('
  var sexprs = newSeq[Sexpr]()
  while true:
    consumeWhitespace(input, opt)
    case input.peekChar
    of ')': break
    else:
      let res = parse(input, opt)
      if res.isNone: raise newException(UnbalancedParensError, "Not enough closing parens")
      sexprs.add(res.get)
  assert input.readChar == ')'
  return Sexpr(line: 0, col: 0, kind: skList, listVal: sexprs)

proc parseSym(input: Stream, opt: ParseOptions): Sexpr =
  var buf = ""
  while true:
    let currChar = input.peekChar
    case currChar
    of IdentChars:
      ## Split if this is in the forceSplitChars set & we've already got some chars in the buf
      let splitChar = opt.forceSplitChars.contains(currChar)
      if splitChar and buf.len > 0: break
      elif splitChar: # Just consume the single force split char
        buf.add(input.readChar)
        break
      buf.add(currChar)
    else: break
    let _ = input.readChar
  assert buf.len > 0
  ## There's a special case, where a symbol is . followed by one or more
  ## numbers. If this is the case, actually return a float (e.g. ".5" == 0.5)
  if buf[0] == '.' and buf.len >= 2 and buf[1..^1].allIt(it in Digits):
    return Sexpr(line: 0, col: 0, kind: skFloat, floatVal: buf.parseFloat)
  else:
    return Sexpr(line: 0, col: 0, kind: skSym, symVal: buf)

proc parseString(input: Stream, opt: ParseOptions): Sexpr =
  assert input.readChar() == '"'
  var buf = ""
  while true:
    let currChar = input.peekChar
    case currChar
    of '\\': raise newException(Exception, "Unimplemented escape chars")
    of '"': break
    else: buf.add(currChar)
    let _ = input.readChar
  return Sexpr(line: 0, col: 0, kind: skString, stringVal: buf)

## Returns none if the input stream is empty
proc parse*(input: Stream, opt: ParseOptions): Option[Sexpr] =
  consumeWhitespace(input, opt)
  case input.peekChar()
  of '\0': return none(Sexpr)
  of '(': return some(parseList(input, opt))
  of Digits: return some(parseNumber(input, opt))
  of '"': return some(parseString(input, opt))
  of ')': raise newException(UnbalancedParensError, "Too many closing parens")
  of IdentStartChars: return some(parseSym(input, opt))
  else: raise newException(UnexpectedCharacterError, fmt"Unexpected character {input.peekChar()}")

## Returns none if the input stream is empty
proc parse*(input: Stream): Option[Sexpr] =
  parse(input, initParseOptions())
