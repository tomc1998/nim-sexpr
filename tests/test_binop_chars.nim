import sexpr
import sequtils
import options
import streams

let opt = initParseOptions().withBinaryOpChar('*')

block:
  let res = parse(newStringStream("1*2"), opt).get
  assert res.kind == skList
  assert res.listVal.len == 3
  assert res.listVal[0].symVal == "*"
  assert res.listVal[1].intVal == 1
  assert res.listVal[2].intVal == 2

block:
  let res = parse(newStringStream("1*2"), opt).get
  assert res.kind == skList
  assert res.listVal.len == 3
  assert res.listVal[0].symVal == "*"
  assert res.listVal[1].intVal == 1
  assert res.listVal[2].intVal == 2

doAssertRaises(IncompleteBinaryOpError):
  let _ = parse(newStringStream("1*"), opt)
