package core.bigrammar

import core.bigrammar.grammars.Labelled
import core.grammar.~
import org.scalatest.FunSuite

class TestSimpleExpressionLanguage extends FunSuite with BiGrammarSequenceWriter {

  test("SimpleAddition") {
    val example = "3 + 4"
    val expected = Add(Value(3), Value(4))

    parseAndPrint(example, expected)
  }

  test("TwoAdditions") {
    val example = "3 + 4 + 2"
    val expected = Add(Value(3), Add(Value(4),Value(2)))

    parseAndPrint(example, expected)
  }

  test("MultiplyWithAddition") {
    val example = "3 * 4 + 2"
    val expected = Add(Multiply(Value(3), Value(4)),Value(2))

    parseAndPrint(example, expected)
  }

  test("MultiplyWithAddition2") {
    val example = "3 + 4 * 2"
    val expected = Add(Value(3), Multiply(Value(4),Value(2)))

    parseAndPrint(example, expected)
  }

  test("MultiplyWithAdditionWithParenthesis") {
    val example = "3 * (4 + 2)"
    val expected = Multiply(Value(3), Add(Value(4),Value(2)))

    parseAndPrint(example, expected)
  }

  test("If") {
    val newLine = System.lineSeparator()
    val example = s"3$newLine? 4$newLine: 2"
    val expected = IfNotZero(Value(3), Value(4),Value(2))

    parseAndPrint(example, expected)
  }

  def parseAndPrint(example: String, expected: Any) {
    val grammarDocument = getExpressionGrammarDocument
    TestGrammarUtils.parseAndPrintSame(example, Some(expected), grammarDocument)
  }

  def getExpressionGrammarDocument: Labelled = {
    val expression = new Labelled(StringKey("expression"))
    val parenthesis: BiGrammar = "(" ~> expression ~< ")"


    val numberValue: BiGrammar = integer ^^ (v => Value(v.asInstanceOf[Int]), {
      case Value(i) => Some(i)
      case _ => None
    })

    val multipleLabel = new Labelled(StringKey("multiply"))
    val multiply = (multipleLabel ~~< "*") ~~ multipleLabel ^^( {
      case core.grammar.~(l, r) => Multiply(l.asInstanceOf[TestExpression], r.asInstanceOf[TestExpression])
    }, {
      case Multiply(l, r) => Some(core.grammar.~(l, r))
      case _ => None
    })
    multipleLabel.addOption(multiply)
    multipleLabel.addOption(numberValue)
    multipleLabel.addOption(parenthesis)

    val addLabel = new Labelled(StringKey("add"))
    val add: BiGrammar = (addLabel ~~< "+") ~~ addLabel ^^( {
      case core.grammar.~(l, r) => Add(l.asInstanceOf[TestExpression], r.asInstanceOf[TestExpression])
    }, {
      case Add(l, r) => Some(core.grammar.~(l, r))
      case _ => None
    })
    addLabel.addOption(add)
    addLabel.addOption(multipleLabel)

    val _if: BiGrammar = expression % ("?" ~~> expression) % (":" ~~> expression) ^^( {
      case cond ~ _then ~ _else => IfNotZero(cond.asInstanceOf[TestExpression], _then.asInstanceOf[TestExpression], _else.asInstanceOf[TestExpression])
    }, {
      case IfNotZero(cond, _then, _else) => Some(core.grammar.~(core.grammar.~(cond, _then), _else))
      case _ => None
    })

    expression.addOption(_if)
    expression.addOption(addLabel)
    expression
  }
}
