package transformations.javac

import core.bigrammar.{BiGrammar, TestGrammarUtils, WithTrivia}
import core.particles.grammars.{GrammarCatalogue, ProgramGrammar}
import core.particles.node.{NodeClass, NodeField}
import core.particles.{Language, NodeGrammarWriter}
import org.scalatest.FunSuite

class TriviaInsideNodeTest extends FunSuite with NodeGrammarWriter {

  object ParentClass extends NodeClass
  object ChildClass extends NodeClass
  object ParentName extends NodeField
  object ParentChild extends NodeField
  object ChildName extends NodeField

  test("Trivia is moved inside Child Node") {
    val grammars = new GrammarCatalogue
    import grammars._

    val grammar: BiGrammar = "ParentStart" ~ identifier.as(ParentName) ~
      ("ChildStart" ~ identifier.as(ChildName) ~ "ChildEnd" asLabelledNode ChildClass).as(ParentChild) ~ "ParentEnd" asLabelledNode ParentClass
    grammars.create(ProgramGrammar, grammar)
    assert(grammars.find(ChildClass).inner != grammars.trivia)
    val input = """ChildStart judith ChildEnd""".stripMargin
    val inputWithSpace = " " + input
    val beforeTransformationWithSpace = TestGrammarUtils.parse(inputWithSpace, grammars.find(ChildClass))
    val beforeTransformation = TestGrammarUtils.parse(input, grammars.find(ChildClass))
    assert(!beforeTransformationWithSpace.successful)
    assert(beforeTransformation.successful, beforeTransformation.toString)
    TriviaInsideNode.transformGrammars(grammars, new Language)
    val afterTransformation = TestGrammarUtils.parse(inputWithSpace, grammars.find(ChildClass))
    assert(afterTransformation.successful, afterTransformation.toString)
  }

  test("No doubles") {
    val grammars = new GrammarCatalogue
    import grammars._

    val parentGrammar = identifier.as(ParentName).asLabelledNode(ParentClass)
    grammars.create(ProgramGrammar, "Start" ~ (parentGrammar | parentGrammar))
    TriviaInsideNode.transformGrammars(grammars, new Language)
    val expectedParentGrammar = new WithTrivia(identifier.as(ParentName)).asLabelledNode(ParentClass)
    assertResult(expectedParentGrammar.toString)(parentGrammar.toString) //TODO use actual equality instead of toString
  }
}
