package core.grammarDocument

import core.grammar.{NumberG, ~}
import org.junit.Test

class TestSimpleExpressionLanguage extends GrammarDocumentWriter {

  @Test
  def testSimpleAddition() {
    val example = "3 + 4"
    val expected = Add(Value(3), Value(4))

    parseAndPrint(example, expected)
  }

  @Test
  def testTwoAdditions() {
    val example = "3 + 4 + 2"
    val expected = Add(Value(3), Add(Value(4),Value(2)))

    parseAndPrint(example, expected)
  }

  @Test
  def testMultiplyWithAddition() {
    val example = "3 * 4 + 2"
    val expected = Add(Multiply(Value(3), Value(4)),Value(2))

    parseAndPrint(example, expected)
  }

  @Test
  def testMultiplyWithAddition2() {
    val example = "3 + 4 * 2"
    val expected = Add(Value(3), Multiply(Value(4),Value(2)))

    parseAndPrint(example, expected)
  }

  @Test
  def testMultiplyWithAdditionWithParenthesis() {
    val example = "3 * (4 + 2)"
    val expected = Multiply(Value(3), Add(Value(4),Value(2)))

    parseAndPrint(example, expected)
  }

  @Test
  def testIf() {
    val example = "3\n? 4\n: 2"
    val expected = IfNotZero(Value(3), Value(4),Value(2))

    parseAndPrint(example, expected)
  }

  def parseAndPrint(example: String, expected: Any) {
    val grammarDocument = getExpressionGrammarDocument
    TestGrammarUtils.parseAndPrint(example, Some(expected), grammarDocument)
  }

  def getExpressionGrammarDocument: Labelled = {
    val expression = new Labelled("expression")
    val parenthesis: GrammarDocument = "(" ~> expression <~ ")"


    val number: GrammarDocument = consume(NumberG) ^^(v => new Value(Integer.parseInt(v.asInstanceOf[String])), {
      case Value(i) => Some(i)
      case _ => None
    })

    val multipleLabel = new Labelled("multiply")
    val multiply = (multipleLabel <~~ "*") ~~ multipleLabel ^^( {
      case core.grammar.~(l, r) => Multiply(l.asInstanceOf[TestExpression], r.asInstanceOf[TestExpression])
    }, {
      case Multiply(l, r) => Some(core.grammar.~(l, r))
      case _ => None
    })
    multipleLabel.orToInner(multiply)
    multipleLabel.orToInner(number)
    multipleLabel.orToInner(parenthesis)

    val addLabel = new Labelled("add")
    val add: GrammarDocument = (addLabel <~~ "+") ~~ addLabel ^^( {
      case core.grammar.~(l, r) => Add(l.asInstanceOf[TestExpression], r.asInstanceOf[TestExpression])
    }, {
      case Add(l, r) => Some(core.grammar.~(l, r))
      case _ => None
    })
    addLabel.orToInner(add)
    addLabel.orToInner(multipleLabel)

    val _if: GrammarDocument = expression % ("?" ~~> expression) % (":" ~~> expression) ^^( {
      case cond ~ then ~ _else => IfNotZero(cond.asInstanceOf[TestExpression], then.asInstanceOf[TestExpression], _else.asInstanceOf[TestExpression])
    }, {
      case IfNotZero(cond, then, _else) => Some(core.grammar.~(core.grammar.~(cond, then), _else))
      case _ => None
    })

    expression.orToInner(_if)
    expression.orToInner(addLabel)
    expression
  }
}
