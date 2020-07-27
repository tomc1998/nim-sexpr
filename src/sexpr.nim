import streams
import options
import strutils
import strformat

const IdentStartChars =
  strutils.IdentStartChars + {'+', '-', '*', '/', ';', '>', '<', '=', '!', '$', '%', '^', '?'}
const IdentChars = IdentStartChars + Digits

type 
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

type UnbalancedParensError = object of CatchableError
type UnexpectedCharacterError = object of CatchableError

proc parse*(input: Stream): Option[Sexpr]

proc consumeWhitespace(input: Stream) =
  while input.peekChar() in Whitespace:
    let _ = input.readChar()

proc parseNumber(input: Stream): Sexpr =
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

proc parseList(input: Stream): Sexpr =
  assert input.readChar() == '('
  var sexprs = newSeq[Sexpr]()
  while true:
    consumeWhitespace(input)
    case input.peekChar
    of ')': break
    else:
      let res = parse(input)
      if res.isNone: raise newException(UnbalancedParensError, "Not enough closing parens")
      sexprs.add(res.get)
  assert input.readChar == ')'
  return Sexpr(line: 0, col: 0, kind: skList, listVal: sexprs)

proc parseSym(input: Stream): Sexpr =
  var buf = ""
  while true:
    let currChar = input.peekChar
    case currChar
    of IdentChars: buf.add(currChar)
    else: break
    let _ = input.readChar
  return Sexpr(line: 0, col: 0, kind: skSym, symVal: buf)

proc parseString(input: Stream): Sexpr =
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
proc parse*(input: Stream): Option[Sexpr] =
  consumeWhitespace(input)
  case input.peekChar()
  of '\0': return none(Sexpr)
  of '(': return some(parseList(input))
  of Digits, '.': return some(parseNumber(input))
  of '"': return some(parseString(input))
  of ')': raise newException(UnbalancedParensError, "Too many closing parens")
  of IdentStartChars: return some(parseSym(input))
  else: raise newException(UnexpectedCharacterError, fmt"Unexpected character {input.peekChar()}")
