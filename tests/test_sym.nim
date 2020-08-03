import sexpr
import options
import streams

block:
  let res = parse(newStringStream("abc ")).get
  assert res.kind == skSym
  assert res.symVal == "abc"

block:
  let res = parse(newStringStream("abc123 ")).get
  assert res.kind == skSym
  assert res.symVal == "abc123"

block:
  let res = parse(newStringStream("kebab-case ")).get
  assert res.kind == skSym
  assert res.symVal == "kebab-case"

block:
  let res = parse(newStringStream("123abc")).get
  assert res.kind != skSym

block:
  let res = parse(newStringStream("a.b")).get
  assert res.kind == skSym
  assert res.symVal == "a.b"

block:
  let res = parse(newStringStream("a.b"), initParseOptions().withForceSplitChar('.')).get
  assert res.kind == skSym
  assert res.symVal == "a"
