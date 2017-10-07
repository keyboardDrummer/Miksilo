package example

import core.bigrammar.{BiGrammar, Keyword, Labelled, TestGrammarUtils}
import core.grammar.~
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{NodeClass, NodeField}
import core.particles.{DeltaWithGrammar, Language}
import org.scalatest.FunSuite

object While {
  object Clazz extends NodeClass
  object Condition extends NodeField
  object Body extends NodeField
}

object PlusEquals {
  object Clazz extends NodeClass
  object Target extends NodeField
  object Value extends NodeField
}

object Decrement {
  object Clazz extends NodeClass
  object Target extends NodeField
}

object Variable {
  object Clazz extends NodeClass
  object Name extends NodeField
}

object Constant {
  object Clazz extends NodeClass
  object Value extends NodeField
}

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

  test("whileWithAsNode") {
    val expression = new Labelled("expression")
    expression.addOption(identifier.as(Variable.Name).asNode(Variable.Clazz))
    expression.addOption(number.as(Constant.Value).asNode(Constant.Clazz))
    val assignment = identifier ~~ keywordClass("=") ~~ expression |
      (identifier.as(PlusEquals.Target) ~~ "+=" ~~ expression.as(PlusEquals.Value)).asNode(PlusEquals.Clazz)
    expression.addOption(assignment)
    expression.addOption(identifier.as(Decrement.Target) ~ "--" asNode Decrement.Clazz)
    expression.addOption(expression ~~ "-" ~~ expression)
    val statement = new Labelled("statement")
    val _while =
      "while" ~ expression.inParenthesis.as(While.Condition) ~~ "{" %
        statement.manyVertical.indent(2).as(While.Body) %<
        "}" asNode While.Clazz

    statement.addOption(_while)
    statement.addOption(expression ~< ";")

    val string =
      """
        |While.Clazz:
        | Map(While.Condition -> Variable.Clazz:
        |       Map(Variable.Name -> i),
        |     While.Body -> List(
        |       Decrement.Clazz:
        |         Map(Decrement.Target -> i),
        |        PlusEquals.Clazz:
        |         Map(PlusEquals.Target -> x,
        |             PlusEquals.Value -> Constant.Clazz:
        |               Map(Constant.Value -> 2)
        |            )
        |     )
        |    )
      """.stripMargin
    val example =
      """while (i){
        |  i--; x += 2;
        |}""".stripMargin

    val expectation =
      """while(i) {
        |  i--;
        |  x += 2;
        |}""".stripMargin
    val grammar = statement.manyVertical
    val parseResult = TestGrammarUtils.parse(example, grammar)
    var result = parseResult.get
    System.out.print(result)
    assertResult(expectation)(TestGrammarUtils.print(result, grammar))
  }

  test("test5") {
    case class Assignment(target: Any, value: Any)
    case class Or(left: Any, right: Any, strict: Boolean)
    object Or extends NodeClass

    val expression: BiGrammar = ???

    val grammar = (expression ~< "=" ~ expression).map[~[Any,Any], Assignment](
      { case core.grammar.~(target: Any, value: Any) => Assignment(target, value) },
      (assignment: Assignment) => Some(new ~(assignment.target, assignment.value)))

    val grammar2 = expression ~< "|" ~ ("|" ~> value(false) | value(true)) ~ expression map[~[~[Any,Boolean],Any], Or](
      { case ~(~(left: Any, strict: Boolean), right: Any) => Or(left, right, strict) },
      (or: Or) => Some(new ~(new ~(or.left, or.strict), or.right)))

    object Left extends NodeField
    object Right extends NodeField
    object Strict extends NodeField

    val strict = ("|" ~> value(false) | value(true)).as(Strict)
    val grammar3 = expression.as(Left) ~< "|" ~ strict ~ expression.as(Right) asNode Or
  }

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = ???

  override def description: String = ???
}
