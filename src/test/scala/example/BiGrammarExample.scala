package example

import core.bigrammar.{BiGrammar, Labelled, TestGrammarUtils}
import core.grammar.~
import core.particles.NodeGrammarWriter
import core.particles.node.{NodeClass, NodeField}
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

/**
  * Contains some examples for the wiki.
  */
class BiGrammarExample extends FunSuite with NodeGrammarWriter {

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
    val result = parseResult.get
    System.out.print(result)
    assertResult(expectation)(TestGrammarUtils.print(result, grammar))
  }

  test("orOperatorWithoutAs") {
    case class Assignment(target: Any, value: Any)
    case class Or(left: Any, right: Any, strict: Boolean)
    object Or extends NodeClass

    val expression: BiGrammar = "true" ~> value(true) | "false" ~> value(false)

    val orGrammar = expression ~< "|" ~ ("|" ~> value(false) | value(true)) ~ expression ^^ (
      { case ~(~(left: Any, strict: Boolean), right: Any) => Or(left, right, strict) },
      { case or: Or => Some(new ~(new ~(or.left, or.strict), or.right)); case _ => None })

    object Left extends NodeField
    object Right extends NodeField
    object Strict extends NodeField

    val strict = ("|" ~> value(false) | value(true)).as(Strict)
    val strictOrGrammarWithAs = expression.as(Left) ~< "|" ~ strict ~ expression.as(Right) asNode Or
  }
}
