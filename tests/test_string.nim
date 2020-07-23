import sexpr
import options
import streams

let res = parse(newStringStream("\"Hello\"")).get
assert res.kind == skString
assert res.stringVal == "Hello"
