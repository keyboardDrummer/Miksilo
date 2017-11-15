package deltas.javac

import core.bigrammar._
import core.bigrammar.grammars.{Labelled, Sequence, WithTrivia}
import core.particles.grammars.BodyGrammar
import core.particles.node.{GrammarKey, NodeClass, NodeField}
import core.particles.{Language, NodeGrammarWriter}
import org.scalatest.FunSuite
import deltas.javac.trivia.TriviaInsideNode

class TriviaInsideNodeTest extends FunSuite with NodeGrammarWriter {

  object ParentClass extends NodeClass
  object ChildClass extends NodeClass
  object ParentName extends NodeField
  object ParentChild extends NodeField
  object ChildName extends NodeField

  test("Trivia is moved inside Child Node") {
    val language = new Language
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
    val language = new Language
    val grammars = language.grammars
    import grammars._

    val parentGrammar = identifier.as(ParentName).asLabelledNode(ParentClass)
    language.grammars.root.inner = "Start" ~ (parentGrammar | parentGrammar)
    TriviaInsideNode.transformGrammars(grammars, new Language)
    val expectedParentGrammar = new WithTrivia(identifier.as(ParentName)).asLabelledNode(ParentClass)
    assertResult(expectedParentGrammar.toString)(parentGrammar.toString) //TODO use actual equality instead of toString
  }

  object IntegerClass extends NodeClass
  object Value extends NodeField
  object Left extends NodeField
  object Right extends NodeField
  object Add extends NodeClass
  object Expression extends GrammarKey
  test("Left Recursive") {
    val language = new Language
    val grammars = language.grammars
    import grammars._

    val numberGrammar = (number : BiGrammar).as(Value).asLabelledNode(IntegerClass)
    val expressionGrammar = new Labelled(Expression)
    val additionGrammar = expressionGrammar.as(Left) ~ "+" ~ expressionGrammar.as(Right) asLabelledNode Add
    expressionGrammar.addOption(numberGrammar)
    expressionGrammar.addOption(additionGrammar)

    grammars.find(BodyGrammar).inner = expressionGrammar
    TriviaInsideNode.transformGrammars(grammars, language)

    val expectedAdditionGrammar = expressionGrammar.as(Left) ~ new Sequence("+", expressionGrammar.as(Right))
    val expectedNumberGrammar = new WithTrivia((number : BiGrammar).as(Value), grammars.trivia)
    assertResult(expectedAdditionGrammar.toString)(additionGrammar.inner.toString) //TODO use actual equality instead of toString
    assertResult(expectedNumberGrammar.toString)(numberGrammar.inner.toString) //TODO use actual equality instead of toString
  }
}
