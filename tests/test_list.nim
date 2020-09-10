import sexpr
import sequtils
import options
import streams

block:
  let res = parse(newStringStream("(1 2 3)")).get
  assert res.kind == skList
  assert res.listVal.len == 3
  assert res.listVal.allIt(it.kind == skInt)
  assert res.listVal[0].intVal == 1
  assert res.listVal[1].intVal == 2
  assert res.listVal[2].intVal == 3

block:
  let res = parse(newStringStream("((1 2 3) (1 2 3) (1 2 3))")).get
  assert res.kind == skList
  assert res.listVal.len == 3
  for x in res.listVal:
    assert x.kind == skList
    assert x.listVal.len == 3
    assert x.listVal.allIt(it.kind == skInt)
    assert x.listVal[0].intVal == 1
    assert x.listVal[1].intVal == 2
    assert x.listVal[2].intVal == 3

block:
  let opt = initParseOptions().withForceSplitChar('.')
  assert parse(newStringStream("(a . b . c)")).get ==
    parse(newStringStream("(a.b.c)"), opt).get
  assert parse(newStringStream("(a . b . c)"), opt).get ==
    parse(newStringStream("(a.b.c)"), opt).get

block:
  let res = parse(newStringStream("[1 2 3]")).get
  assert res.kind == skSquareList
  assert res.squareListVal.len == 3
  assert res.squareListVal.allIt(it.kind == skInt)
  assert res.squareListVal[0].intVal == 1
  assert res.squareListVal[1].intVal == 2
  assert res.squareListVal[2].intVal == 3

