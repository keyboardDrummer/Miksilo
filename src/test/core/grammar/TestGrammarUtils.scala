package core.grammar

import core.transformation._

class TestGrammarUtils {

  val manager: TransformationManager = new TransformationManager()

  def buildParser(transformations: Seq[GrammarTransformation], selector: Grammar => Grammar): String => manager.ParseResult[Any] = {

    object SelectorTransformation extends GrammarTransformation {
      override def transformGrammar(grammar: Grammar): Grammar = selector(grammar)

      override def transform(program: MetaObject, state: TransformationState): Unit = {}

      override def dependencies: Set[ProgramTransformation] = Set.empty
    }

    manager.buildParser(transformations ++ Seq(SelectorTransformation))
  }
}
