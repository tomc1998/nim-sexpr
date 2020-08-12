import streams
import sets
import options
import sequtils
import strutils
import strformat

const IdentStartChars =
  strutils.IdentStartChars + {'+', '-', '*', '/', ';', ':', '`', '>', '<', '=', '!', '$', '%', '^', '?', '&', '.', '\''}
const IdentChars = IdentStartChars + Digits

type 
  ParseOptions* = object
    ## A list of characters to force the parser to split on. For example, if
    ## '.' is present in this list, then `foo.bar` will become 3 separate
    ## symbols, `foo`, `.`, `bar` - whereas without '.' being present, this
    ## will be parsed as a single symbol - `foo.bar`.
    forceSplitChars: HashSet[char]
    ## For each char in this set, convert foo # bar (where '#' is the char in
    ## this case) to (# foo bar).
    ## This is left-associative, so foo # bar # baz == (# (# foo bar) baz).
    ## This is useful for 'dot-notation' syntax, e.g. 'foo.bar.baz'.
    ## It is implied that a character in this set is a 'force split char' - it
    ## does *not* need to be specified in forceSplitChars as well.
    binaryOpChars: HashSet[char]
    ## Similar to binaryOpChars, but converts #x to (# x). Useful for & for
    ## address-of operations, so you can write &int instead of (ref int) or
    ## something.
    ## In this case, a unary op does NOT count as a forceSplitChar, so if & is
    ## a unary op char, abc&def is still a single identifier. On the other
    ## hand, an identifier can no longer start with a char in unaryOpChars -
    ## &asd will count as (& asd), not a single ident &asd.
    ## Unary ops take precedence over binops, so &x * &y will be 
    ## (* (& x) (& ## y)) 
    ## rather than 
    ## (& (* x (& y)))
    unaryOpChars: HashSet[char]
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

proc toReadable*(s: Sexpr): string =
  case s.kind
  of skInt: repr s.intVal
  of skFloat: repr s.floatVal
  of skString: "\""&s.stringVal&"\""
  of skSym: s.symVal
  of skList:
    let listItems = s.listVal.map(toReadable).join(" ")
    fmt"({listItems})"

proc `==`*(a: Sexpr, b: Sexpr): bool =
  if a.kind != b.kind: return false
  case a.kind
  of skInt: a.intVal == b.intVal
  of skFloat: a.floatVal == b.floatVal
  of skString: a.stringVal == b.stringVal
  of skSym: a.symVal == b.symVal
  of skList: a.listVal == b.listVal

type UnbalancedParensError* = object of CatchableError
type UnexpectedCharacterError* = object of CatchableError
## For when a char in binaryOpChars is not followed by a valid sexpr
type IncompleteBinaryOpError* = object of CatchableError

proc initParseOptions*(): ParseOptions = ParseOptions(forceSplitChars: initHashSet[char]())
proc withForceSplitChar*(p: ParseOptions, c: char): ParseOptions =
  var p = p
  p.forceSplitChars.incl(c)
  p
proc withBinaryOpChar*(p: ParseOptions, c: char): ParseOptions =
  var p = p
  p.binaryOpChars.incl(c)
  p
proc withUnaryOpChar*(p: ParseOptions, c: char): ParseOptions =
  var p = p
  p.unaryOpChars.incl(c)
  p
## Return true if we should split on this char with the given parse options
proc shouldSplitOn(p: ParseOptions, c: char): bool =
  p.forceSplitChars.contains(c) or p.binaryOpChars.contains(c)

proc parse*(input: Stream, opt: ParseOptions): Option[Sexpr]
proc parse*(input: Stream, opt: ParseOptions, lAssoc: bool): Option[Sexpr]

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
      ## Check if we should split here
      let splitChar = opt.shouldSplitOn(currChar)
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

## Peeks the next char after whitespace, if it's a binary op char, then wrap up
## lhs and return a listVal (# lhs rhs).
proc tryBinop(input: Stream, opt: ParseOptions, lhs: Sexpr): Option[Sexpr] =
  consumeWhitespace(input, opt)
  let peek = input.peekChar
  if opt.binaryOpChars.contains(peek):
    let _ = input.readChar
    ## Consume more here and return a (# a b) instead of a # b
    consumeWhitespace(input, opt)
    let rhs = parse(input, opt)
    if rhs.isNone:
      raise newException(IncompleteBinaryOpError,
        fmt"Expected value to complete the binary expression with operator '{peek}'")
    return some(Sexpr(line: 0, col: 0, kind: skList, listVal: @[
      Sexpr(line: 0, col: 0, kind: skSym, symVal: $peek),
      lhs,
      rhs.get]))
  return none(Sexpr)

proc parseUnaryOp(input: Stream, opt: ParseOptions): Sexpr =
  let unaryOp = Sexpr(line: 0, col: 0, kind: skSym, symVal: $input.readChar)
  let target = parse(input, opt, true).get
  Sexpr(line: 0, col: 0, kind: skList, listVal: @[unaryOp, target])

## @param lAssoc - set to true to ignore any trailing binary ops. Useful for
## unary ops, which take precedence over bin ops.
proc parse*(input: Stream, opt: ParseOptions, lAssoc: bool): Option[Sexpr] =
  consumeWhitespace(input, opt)
  let peek = input.peekChar()
  var ret =
    if peek in opt.unaryOpChars:
      some(parseUnaryOp(input, opt))
    else:
      case input.peekChar()
        of '\0': none(Sexpr)
        of '(': some(parseList(input, opt))
        of Digits: some(parseNumber(input, opt))
        of '"': some(parseString(input, opt))
        of ')': raise newException(UnbalancedParensError, "Too many closing parens")
        of IdentStartChars: some(parseSym(input, opt))
        else: raise newException(UnexpectedCharacterError, fmt"Unexpected character {input.peekChar()}")
  if lAssoc or ret.isNone: return ret
  var binop = tryBinop(input, opt, ret.get)
  while binop.isSome:
    ret = binop
    binop = tryBinop(input, opt, ret.get)
  ret
proc parse*(input: Stream, opt: ParseOptions): Option[Sexpr] = parse(input, opt, false)
## Returns none if the input stream is empty
proc parse*(input: Stream): Option[Sexpr] =
  parse(input, initParseOptions())
