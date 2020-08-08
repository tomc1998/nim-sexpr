import sexpr
import options
import streams

let testStrings = @[
  "(1 2 3)",
  "(1.0 2.0 3.0)",
  "\"Hello\"",
  "1",
  "hello-world"]

for x in testStrings:
  assert parse(newStringStream(x)).get.toReadable == x
