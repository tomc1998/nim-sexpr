import sexpr
import options
import streams

block:
  let res = parse(newStringStream("1")).get
  assert res.kind == skInt
  assert res.intVal == 1

block:
  let res = parse(newStringStream("123")).get
  assert res.kind == skInt
  assert res.intVal == 123

block:
  let res = parse(newStringStream("0123")).get
  assert res.kind == skInt
  assert res.intVal == 123

block:
  let res = parse(newStringStream("0xa")).get
  assert res.kind == skInt
  assert res.intVal == 10

block:
  let res = parse(newStringStream("0b101")).get
  assert res.kind == skInt
  assert res.intVal == 5

block:
  let res = parse(newStringStream(".5")).get
  assert res.kind == skFloat
  assert res.floatVal == 0.5

block:
  let res = parse(newStringStream("5.")).get
  assert res.kind == skFloat
  assert res.floatVal == 5.0
