package transformations.javac

import core.bigrammar.{BiGrammar, TestGrammarUtils}
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
    val language1 = new Language

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
    TriviaInsideNode.transformGrammars(grammars, language1)
    val afterTransformation = TestGrammarUtils.parse(inputWithSpace, grammars.find(ChildClass))
    assert(afterTransformation.successful, afterTransformation.toString)
  }
}
