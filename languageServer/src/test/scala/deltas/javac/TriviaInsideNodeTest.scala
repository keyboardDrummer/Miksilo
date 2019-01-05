package deltas.javac

import core.bigrammar._
import core.bigrammar.grammars.{Labelled, BiSequence, WithTrivia}
import core.deltas.NodeGrammarWriter
import core.deltas.grammars.BodyGrammar
import core.language.Language
import core.language.node.{GrammarKey, NodeField, NodeShape}
import deltas.trivia.TriviaInsideNode
import org.scalatest.FunSuite

class TriviaInsideNodeTest extends FunSuite with NodeGrammarWriter {

  object ParentClass extends NodeShape
  object ChildClass extends NodeShape
  object ParentName extends NodeField
  object ParentChild extends NodeField
  object ChildName extends NodeField

  test("Trivia is moved inside Child Node") {
    val language = new Language()
    val grammars = language.grammars
    import grammars._

    val grammar: BiGrammar = "ParentStart" ~ identifier.as(ParentName) ~
      ("ChildStart" ~ identifier.as(ChildName) ~ "ChildEnd" asLabelledNode ChildClass).as(ParentChild) ~ "ParentEnd" asLabelledNode ParentClass
    language.grammars.root.inner = grammar
    assert(grammars.find(ChildClass).inner != grammars.trivia)
    val input = """ChildStart judith ChildEnd""".stripMargin
    val inputWithSpace = " " + input
    val beforeTransformationWithSpace = TestGrammarUtils.parse(inputWithSpace, grammars.find(ChildClass))
    val beforeTransformation = TestGrammarUtils.parse(input, grammars.find(ChildClass))
    assert(!beforeTransformationWithSpace.successful)
    assert(beforeTransformation.successful, beforeTransformation.toString)
    TriviaInsideNode.transformGrammars(grammars, language)
    val afterTransformation = TestGrammarUtils.parse(inputWithSpace, grammars.find(ChildClass))
    assert(afterTransformation.successful, afterTransformation.toString)
  }

  test("No doubles") {
    val language = new Language()
    val grammars = language.grammars
    import grammars._

    val parentGrammar = identifier.as(ParentName).asLabelledNode(ParentClass)
    language.grammars.root.inner = "Start" ~ (parentGrammar | parentGrammar)
    TriviaInsideNode.transformGrammars(grammars, new Language())
    val expectedParentGrammar = new WithTrivia(identifier.as(ParentName)).asLabelledNode(ParentClass)
    assertResult(expectedParentGrammar.toString)(parentGrammar.toString) //TODO use actual equality instead of toString
  }

  object IntegerClass extends NodeShape
  object Value extends NodeField
  object Left extends NodeField
  object Right extends NodeField
  object Add extends NodeShape
  object Expression extends GrammarKey

  test("Left Recursive") {
    val language = new Language()
    val grammars = language.grammars
    import grammars._

    val numberGrammar = (number : BiGrammar).as(Value).asLabelledNode(IntegerClass)
    val expressionGrammar = new Labelled(Expression)
    val additionGrammar = expressionGrammar.as(Left) ~ "+" ~ expressionGrammar.as(Right) asLabelledNode Add
    expressionGrammar.addAlternative(numberGrammar)
    expressionGrammar.addAlternative(additionGrammar)

    def lr(l: BiGrammar, r: BiGrammar) = leftRight(l, r, BiSequence.identity)
    grammars.find(BodyGrammar).inner = expressionGrammar
    val expectedBeforeAdditionGrammar = lr(expressionGrammar.as(Left), new WithTrivia(lr("+",
      new WithTrivia(expressionGrammar.as(Right), grammars.trivia)), grammars.trivia))
    val expectedBeforeNumberGrammar = (number : BiGrammar).as(Value)
    assertResult(expectedBeforeAdditionGrammar.toString)(additionGrammar.inner.toString) //TODO use actual equality instead of toString
    assertResult(expectedBeforeNumberGrammar.toString)(numberGrammar.inner.toString) //TODO use actual equality instead of toString
    TriviaInsideNode.transformGrammars(grammars, language)

    val expectedAdditionGrammar = expressionGrammar.as(Left) ~ lr("+", expressionGrammar.as(Right))
    val expectedNumberGrammar = new WithTrivia((number : BiGrammar).as(Value), grammars.trivia)
    assertResult("1" + expectedAdditionGrammar.toString)("1" + additionGrammar.inner.toString) //TODO use actual equality instead of toString
    assertResult("2" + expectedNumberGrammar.toString)("2" + numberGrammar.inner.toString) //TODO use actual equality instead of toString
  }
}
