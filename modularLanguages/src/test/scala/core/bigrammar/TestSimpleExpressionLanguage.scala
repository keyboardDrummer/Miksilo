package core.bigrammar

import org.scalatest.FunSuite

class TestSimpleExpressionLanguage extends FunSuite with WhitespaceTriviaSequenceCombinators {

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

  test("MultiplyWithAdditionWithParenthesisFailure") {
    val example = "3 * (4 + 2)"
    val expected = Multiply(Value(3), Add(Value(4), UndefinedExpression))

    val grammarDocument = getExpressionGrammarDocument

    val expectedError = """could not deconstruct value
                          |Value: (WithMap(Undefined,Map()),Map())
                          |Grammar: multiply TriviasGrammar * TriviasGrammar multiply | number | ( TriviasGrammar expression TriviasGrammar )
                          |Value: (WithMap(Add(Value(4),Undefined),Map()),Map())
                          |Grammar: add TriviasGrammar + TriviasGrammar add | multiply
                          |Value: (WithMap(Multiply(Value(3),Add(Value(4),Undefined)),Map()),Map())
                          |Grammar: multiply TriviasGrammar * TriviasGrammar multiply | number | ( TriviasGrammar expression TriviasGrammar )
                          |Value: (WithMap(Multiply(Value(3),Add(Value(4),Undefined)),Map()),Map())
                          |Grammar: add TriviasGrammar + TriviasGrammar add | multiply
                          |Value: (WithMap(Multiply(Value(3),Add(Value(4),Undefined)),Map()),Map())
                          |Grammar: multiply TriviasGrammar * TriviasGrammar multiply | number | ( TriviasGrammar expression TriviasGrammar )
                          |Value: (WithMap(Multiply(Value(3),Add(Value(4),Undefined)),Map()),Map())
                          |Grammar: add TriviasGrammar + TriviasGrammar add | multiply
                          |Value: (WithMap(Multiply(Value(3),Add(Value(4),Undefined)),Map()),Map())
                          |Grammar: expression % TriviasGrammar % ? TriviasGrammar expression % TriviasGrammar % : TriviasGrammar expression | add
                          |Depth: 25
                          |Partial:
                          |    (3 * (4 + (""".stripMargin

    try
    {
      TestGrammarUtils.print(expected, grammarDocument)
      //noinspection NameBooleanParameters
      assert(false)
    } catch {
      case e: PrintError =>
        assertResult(expectedError)(e.toString)
      case e: Throwable => throw e
    }
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

    val numberValue: BiGrammar = integer.map[Int, Value](v => Value(v), value => value.value)

    val multipleLabel = new Labelled(StringKey("multiply"))
    val multiply = (multipleLabel ~~< "*" ~~ multipleLabel).map[(TestExpression, TestExpression), Multiply](
      t => Multiply(t._1, t._2)
    , multiply => (multiply.first, multiply.second))

    multipleLabel.addAlternative(multiply)
    multipleLabel.addAlternative(numberValue)
    multipleLabel.addAlternative(parenthesis)

    val addLabel = new Labelled(StringKey("add"))
    val add: BiGrammar = (addLabel ~~< "+" ~~ addLabel).map[(TestExpression, TestExpression), Add](
      t => Add(t._1, t._2)
      , add => (add.first, add.second))
    addLabel.addAlternative(add)
    addLabel.addAlternative(multipleLabel)

    val _if: BiGrammar = (expression % ("?" ~~> expression) % (":" ~~> expression)).map[((TestExpression, TestExpression), TestExpression), IfNotZero](
      t => IfNotZero(t._1._1, t._1._2, t._2),
      ifNotZero => ((ifNotZero.condition, ifNotZero._then), ifNotZero._else))

    expression.addAlternative(_if)
    expression.addAlternative(addLabel)
    expression
  }
}
