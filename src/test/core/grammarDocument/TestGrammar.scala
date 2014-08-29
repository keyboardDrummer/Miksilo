package core.grammarDocument

import core.grammar.{Grammar, NumberG, ToPackrat}
import org.junit.{Assert, Test}

import scala.util.parsing.input.CharArrayReader

class TestGrammar extends GrammarDocumentWriter {

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

  def parseAndPrint(example: String, expected: TestExpression) {
    val grammarDocument = getExpressionGrammarDocument
    val grammar: Grammar = ToGrammar.toGrammar(grammarDocument)

    val packrat: ToPackrat = new ToPackrat()
    val result = packrat.phrase(packrat.convert(grammar))(new CharArrayReader(example.toCharArray)).get

    Assert.assertEquals(expected, result)

    val documentResult = ToPrint.toDocument(result, grammarDocument).get.renderString
    Assert.assertEquals(example, documentResult)
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

    expression.orToInner(addLabel)
    expression
  }
}
