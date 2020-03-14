package miksilo.editorParser.parsers

import org.scalatest.funsuite.AnyFunSuite
import editorParsers.LeftRecursiveCorrectingParserWriter
import miksilo.editorParser.parsers.strings.CommonStringReaderParser

class AssociativityTest extends AnyFunSuite with CommonStringReaderParser with LeftRecursiveCorrectingParserWriter {

  test("binary operators are right associative by default") {
    lazy val expr: Parser[Any] = new Lazy(expr) ~< "-" ~ expr | wholeNumber
    val input = "1-2-3"
    val result = expr.getWholeInputParser().parse(input)
    assert(result.successful)
    assertResult(("1",("2","3")))(result.get)
  }

  test("binary operators can be made left associative") {
    lazy val expr: Parser[Any] = wholeNumber.addAlternative[Any]((before, after) => after ~< "-" ~ before)
    val input = "1-2-3"
    val result = expr.getWholeInputParser().parse(input)
    assert(result.successful)
    assertResult((("1","2"),"3"))(result.get)
  }

  test("binary operators can be explicitly right associative") {
    lazy val expr: SequenceParserExtensions[Any] = wholeNumber.addAlternative[Any]((before, after) => before ~< "-" ~ after)
    val input = "1-2-3"
    val result = expr.getWholeInputParser().parse(input)
    assert(result.successful)
    assertResult(("1",("2","3")))(result.get)
  }

  test("if-then-else can be made right-associative") {
    lazy val expr = wholeNumber
    val stmt: Parser[Any] = expr.
      addAlternative[Any]((before, after) => "if{" ~> expr ~ "then{" ~ after ~ "else{" ~ before).
      addAlternative[Any]((before, after) => "if{" ~> expr ~ "then{" ~ after)
    val input = "if{1then{if{2then{3else{4"
    val result = stmt.getWholeInputParser().parse(input)
    assert(result.successful)
    val nestedIf = (((("2", "then{"), "3"), "else{"), "4")
    assertResult((("1","then{"),nestedIf))(result.get)

    val input2 = "if{1then{if{2then{3else{4else{5"
    val result2 = stmt.getWholeInputParser().parse(input2)
    assert(result2.successful)

    val input3 = "if{1then{if{2then{3"
    val result3 = stmt.getWholeInputParser().parse(input3)
    assert(result3.successful)
  }

  test("if-then-else is left-associative by default") {
    lazy val expr = wholeNumber
    lazy val stmt: Parser[Any] = expr |
      "if{" ~> expr ~ "then{" ~ stmt ~ "else{" ~ stmt |
      "if{" ~> expr ~ "then{" ~ stmt
    val input = "if{1then{if{2then{3else{4"
    val result = stmt.getWholeInputParser().parse(input)
    assert(result.successful)

    val nestedIf = (("2", "then{"), "3")
    assertResult((((("1","then{"),nestedIf),"else{"),"4"))(result.get)
  }

  test("if-then-else can be made left-associative") {
    lazy val expr = wholeNumber
    val stmt: Parser[Any] = expr.
      addAlternative[Any]((before, after) => "if{" ~> expr ~ "then{" ~ after).
      addAlternative[Any]((before, after) => "if{" ~> expr ~ "then{" ~ after ~ "else{" ~ after)
    val input = "if{1then{if{2then{3else{4"
    val result = stmt.getWholeInputParser().parse(input)
    assert(result.successful)
    val nestedIf = (("2", "then{"), "3")
    assertResult((((("1","then{"),nestedIf),"else{"),"4"))(result.get)

    val input2 = "if{1then{if{2then{3else{4else{5"
    val result2 = stmt.getWholeInputParser().parse(input2)
    assert(result2.successful)
  }
}
