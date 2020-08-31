import sexpr
import sequtils
import options
import streams

let opt = initParseOptions().withUnaryOpChar('&').withBinaryOpChar('*')

block:
  let res = parse(newStringStream("&x"), opt).get
  assert res.kind == skList
  assert res.listVal.len == 2
  assert res.listVal[0].symVal == "&"
  assert res.listVal[1].symVal == "x"

block:
  let res = parse(newStringStream("&xyz"), opt).get
  assert res.kind == skList
  assert res.listVal.len == 2
  assert res.listVal[0].symVal == "&"
  assert res.listVal[1].symVal == "xyz"

block:
  let res = parse(newStringStream("&x&z"), opt).get
  assert res.kind == skList
  assert res.listVal.len == 2
  assert res.listVal[0].symVal == "&"
  assert res.listVal[1].symVal == "x&z"

block:
  let res = parse(newStringStream("&x*&z"), opt).get
  assert res.kind == skList
  assert res.listVal.len == 3
  assert res.listVal[0].symVal == "*"
  assert res.listVal[1].kind == skList
  assert res.listVal[1].listVal.len == 2
  assert res.listVal[1].listVal[0].symVal == "&"
  assert res.listVal[1].listVal[1].symVal == "x"
  assert res.listVal[2].kind == skList
  assert res.listVal[2].listVal.len == 2
  assert res.listVal[2].listVal[0].symVal == "&"
  assert res.listVal[2].listVal[1].symVal == "z"
