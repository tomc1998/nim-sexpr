import sexpr
import options
import streams

block:
  let res = parse(newStringStream(
  """((1 
2 3
) 4)""")).get
  assert res.kind == skList
  assert res.line == 1 and res.col == 1
  assert res.listVal[0].line == 1 and res.listVal[0].col == 2
  assert res.listVal[1].line == 3 and res.listVal[1].col == 3
  assert res.listVal[0].listVal[0].line == 1 and res.listVal[0].listVal[0].col == 3
  assert res.listVal[0].listVal[1].line == 2 and res.listVal[0].listVal[1].col == 1
  assert res.listVal[0].listVal[2].line == 2 and res.listVal[0].listVal[2].col == 3
