package example

import core.bigrammar.{GrammarDocumentWriter, Labelled, TestGrammarUtils}
import org.scalatest.FunSuite

class BiGrammar extends FunSuite with GrammarDocumentWriter {

  test("test") {
    val expression = new Labelled("expression")
    expression.addOption(expression ~~< "-" ~~ expression)
    expression.addOption(identifier)
    expression.addOption(number)
    val statement = new Labelled("statement")
    val _while =
      "while" ~> expression.inParenthesis ~~< "{" %
        statement.manyVertical.indent(2) %<
        "}"
    statement.addOption(_while)
    val assignment = identifier ~~< "=" ~~ expression
    statement.addOption(assignment)

    val example =
      """while(x) {
        |  x = x - 1
        |}""".stripMargin
    val result = TestGrammarUtils.parseAndPrintSame(example, None, statement.manyVertical)
    System.out.print(result)
  }
}
