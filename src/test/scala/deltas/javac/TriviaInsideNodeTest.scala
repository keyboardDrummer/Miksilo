package deltas.javac

import core.bigrammar._
import core.bigrammar.grammars.{Labelled, LeftRight, WithTrivia}
import core.deltas.grammars.BodyGrammar
import core.deltas.node.{GrammarKey, NodeField, NodeShape}
import core.deltas.{Language, NodeGrammarWriter}
import deltas.javac.trivia.TriviaInsideNode
import org.scalatest.FunSuite

class TriviaInsideNodeTest extends FunSuite with NodeGrammarWriter {

  object ParentClass extends NodeShape
  object ChildClass extends NodeShape
  object ParentName extends NodeField
  object ParentChild extends NodeField
  object ChildName extends NodeField

//  test("Trivia is moved inside Child Node") {
//    val language = new Language(Seq.empty)
//    val grammars = language.grammars
//    import grammars._
//
//    val grammar: BiGrammar = "ParentStart" ~ identifier.as(ParentName) ~
//      ("ChildStart" ~ identifier.as(ChildName) ~ "ChildEnd" asLabelledNode ChildClass).as(ParentChild) ~ "ParentEnd" asLabelledNode ParentClass
//    language.grammars.root.inner = grammar
//    assert(grammars.find(ChildClass).inner != grammars.trivia)
//    val input = """ChildStart judith ChildEnd""".stripMargin
//    val inputWithSpace = " " + input
//    val beforeTransformationWithSpace = TestGrammarUtils.parse(inputWithSpace, grammars.find(ChildClass))
//    val beforeTransformation = TestGrammarUtils.parse(input, grammars.find(ChildClass))
//    assert(!beforeTransformationWithSpace.successful)
//    assert(beforeTransformation.successful, beforeTransformation.toString)
//    TriviaInsideNode.transformGrammars(grammars, language)
//    val afterTransformation = TestGrammarUtils.parse(inputWithSpace, grammars.find(ChildClass))
//    assert(afterTransformation.successful, afterTransformation.toString)
//  }
//
//  test("No doubles") {
//    val language = new Language(Seq.empty)
//    val grammars = language.grammars
//    import grammars._
//
//    val parentGrammar = identifier.as(ParentName).asLabelledNode(ParentClass)
//    language.grammars.root.inner = "Start" ~ (parentGrammar | parentGrammar)
//    TriviaInsideNode.transformGrammars(grammars, new Language(Seq.empty))
//    val expectedParentGrammar = new WithTrivia(identifier.as(ParentName)).asLabelledNode(ParentClass)
//    assertResult(expectedParentGrammar.toString)(parentGrammar.toString) //TODO use actual equality instead of toString
//  }

  object IntegerClass extends NodeShape
  object Value extends NodeField
  object Left extends NodeField
  object Right extends NodeField
  object Add extends NodeShape
  object Expression extends GrammarKey

  test("Left Recursive") {
    val language = new Language(Seq.empty)
    val grammars = language.grammars
    import grammars._

    val numberGrammar = (number : BiGrammar).as(Value).asLabelledNode(IntegerClass)
    val expressionGrammar = new Labelled(Expression)
    val additionGrammar = expressionGrammar.as(Left) ~ "+" ~ expressionGrammar.as(Right) asLabelledNode Add
    expressionGrammar.addOption(numberGrammar)
    expressionGrammar.addOption(additionGrammar)

    grammars.find(BodyGrammar).inner = expressionGrammar
    val expectedBeforeAdditionGrammar = new LeftRight(expressionGrammar.as(Left), new WithTrivia(new LeftRight("+",
      new WithTrivia(expressionGrammar.as(Right), grammars.trivia)), grammars.trivia))
    val expectedBeforeNumberGrammar = (number : BiGrammar).as(Value)
    assertResult(expectedBeforeAdditionGrammar.toString)(additionGrammar.inner.toString) //TODO use actual equality instead of toString
    assertResult(expectedBeforeNumberGrammar.toString)(numberGrammar.inner.toString) //TODO use actual equality instead of toString
    TriviaInsideNode.transformGrammars(grammars, language)

    val expectedAdditionGrammar = expressionGrammar.as(Left) ~ new LeftRight("+", expressionGrammar.as(Right))
    val expectedNumberGrammar = new WithTrivia((number : BiGrammar).as(Value), grammars.trivia)
    assertResult("1" + expectedAdditionGrammar.toString)("1" + additionGrammar.inner.toString) //TODO use actual equality instead of toString
    assertResult("2" + expectedNumberGrammar.toString)("2" + numberGrammar.inner.toString) //TODO use actual equality instead of toString

    //Expected "2[TriviasGrammar ](number).As(TriviaIn...", but got "2[](number).As(TriviaIn..." (TriviaInsideNodeTest.scala:80)
    //De TriviasGrammar is niet ge-insert bij number terwijl die wel is weggehaald bij de rechterhand v.d. +.
  }
}
