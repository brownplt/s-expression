open! SExpression

let test_parse = (~ignoreLangLine=false, str, wanted_result) => {
  let result = {
    switch SExpr.fromString(~ignoreLangLine, str) {
    | elms => Array.join(elms->List.map(SExpr.toString)->List.toArray, " ")
    | exception SExpressionError(err) => `Error: ${Error.toString(err)}`
    }
  }
  if result != wanted_result {
    Js.Console.log(`Test failed:`)
    Js.Console.log(`  Input:`)
    Js.Console.log(`     ${str}`)
    Js.Console.log(`  Wanted result:`)
    Js.Console.log(`     ${wanted_result}`)
    Js.Console.log(`  Actual result:`)
    Js.Console.log(`     ${result}`)
    Js.Console.log(`----------------`)
  }
}

test_parse("(", "Error: reached the end of the file while processing a list.")
test_parse("\"", "Error: reached the end of the file while processing a string.")
test_parse(
  "(]",
  "Error: found a closing square bracket while processing a list started with a round bracket.",
)
test_parse(
  "[)",
  "Error: found a closing round bracket while processing a list started with a square bracket.",
)
test_parse("()", "()")
test_parse("[]", "[]")
test_parse(")", "Error: found an extra closing round bracket at 1:1.")
test_parse("]", "Error: found an extra closing square bracket at 1:1.")
test_parse("#t", "#t")
test_parse("#f", "#f")
test_parse("42", "42")
test_parse("\"foo\" \"bar\"", "\"foo\" \"bar\"")
test_parse("((a) () #t 42)", "((a) () #t 42)")
test_parse("\"\\n\"", "\"\n\"")
test_parse("\"\\t\"", "\"\t\"")
test_parse("\"\\?\"", "Error: found an unexpected escape sequence (\\?).")
// test comments
test_parse("#;(ignore this s-expression) 2 3", "2 3")
test_parse(`
;; ignroe this line
2
3
`, "2 3")
// test #lang
test_parse(~ignoreLangLine=true, "#lang\n2 3", "2 3")
test_parse(~ignoreLangLine=true, `



#lang foo

2
3
`, "2 3")