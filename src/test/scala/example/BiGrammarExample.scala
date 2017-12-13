package example

import core.bigrammar._
import core.bigrammar.grammars.Labelled
import core.grammar.~
import core.deltas.NodeGrammarWriter
import core.deltas.node.{NodeShape, NodeField}
import org.scalatest.FunSuite

object While {
  object Shape extends NodeShape
  object Condition extends NodeField
  object Body extends NodeField
}

object PlusEquals {
  object Shape extends NodeShape
  object Target extends NodeField
  object Value extends NodeField
}

object Decrement {
  object Shape extends NodeShape
  object Target extends NodeField
}

object Variable {
  object Shape extends NodeShape
  object Name extends NodeField
}

object Constant {
  object Shape extends NodeShape
  object Value extends NodeField
}

/**
  * Contains some examples for the wiki.
  */
class BiGrammarExample extends FunSuite with NodeGrammarWriter with BiGrammarSequenceWriter {

  test("whileWithAsNode") {
    val expression = new Labelled(StringKey("expression"))
    expression.addOption(identifier.as(Variable.Name).asNode(Variable.Shape))
    expression.addOption(number.as(Constant.Value).asNode(Constant.Shape))
    val assignment = identifier ~~ keywordClass("=") ~~ expression |
      (identifier.as(PlusEquals.Target) ~~ "+=" ~~ expression.as(PlusEquals.Value)).asNode(PlusEquals.Shape)
    expression.addOption(assignment)
    expression.addOption(identifier.as(Decrement.Target) ~ "--" asNode Decrement.Shape)
    expression.addOption(expression ~~ "-" ~~ expression)
    val statement = new Labelled(StringKey("statement"))
    val _while =
      "while" ~ expression.inParenthesis.as(While.Condition) ~~ "{" %
        statement.manyVertical.indent(2).as(While.Body) %<
        "}" asNode While.Shape

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
    object Or extends NodeShape

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
