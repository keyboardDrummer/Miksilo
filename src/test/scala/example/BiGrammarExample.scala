package example

import core.bigrammar.{BiGrammar, Keyword, Labelled, TestGrammarUtils}
import core.particles.grammars.GrammarCatalogue
import core.particles.{DeltaWithGrammar, Language}
import org.scalatest.FunSuite

class BiGrammarExample extends FunSuite with DeltaWithGrammar {

  test("test") {
    val expression: BiGrammar = "i"
    val statement: BiGrammar = new Keyword("i--;", verifyWhenPrinting = true) | "x += 2;"
    val _while =
      "while" ~> expression.inParenthesis ~~< "{" %
        statement.manyVertical.indent(2) %<
        "}"

    val example =
      """while (i){
        |  i--; x += 2;
        |}""".stripMargin

    val expectation =
      """while(i) {
        |  i--;
        |  x += 2;
        |}""".stripMargin
    val result = TestGrammarUtils.parseAndPrint(example, None, _while)
    assertResult(expectation)(result)
    System.out.print(result)
  }

  test("test2") {
    val expression = new Labelled("expression")
    expression.addOption(identifier)
    expression.addOption(number)
    val assignment = identifier ~~ keywordClass("=") ~~ expression | identifier ~~ keywordClass("+=") ~~ expression
    expression.addOption(assignment)
    expression.addOption(identifier ~ "--")
    expression.addOption(expression ~~ "-" ~~ expression)
    val statement = new Labelled("statement")
    val _while =
      "while" ~ expression.inParenthesis ~~ "{" %
        statement.manyVertical.indent(2) %
        "}"
    statement.addOption(_while)
    statement.addOption(expression ~ ";")

    val example =
      """while (i){
        |  i--; x += 2;
        |}""".stripMargin

    val expectation =
      """while(i) {
        |  i--;
        |  x += 2;
        |}""".stripMargin
    val result = TestGrammarUtils.parseAndPrint(example, None, statement.manyVertical)
    assertResult(expectation)(result)
    System.out.print(result)
  }

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = ???

  override def description: String = ???
}
