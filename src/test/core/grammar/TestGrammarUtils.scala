package core.grammar

import core.transformation.TransformationManager.ProgramGrammar
import core.transformation._

class TestGrammarUtils {

  val manager: TransformationManager = new TransformationManager()

  def buildParser(transformations: Seq[GrammarTransformation], key: Any): String => manager.ParseResult[Any] = {

    object SelectorTransformation extends GrammarTransformation {
      override def transformGrammars(grammars: GrammarCatalogue) {
        grammars.find(ProgramGrammar).inner = grammars.find(key).inner
      }

      override def transform(program: MetaObject, state: TransformationState): Unit = {}

      override def dependencies: Set[Contract] = Set.empty
    }

    manager.buildParser(transformations ++ Seq(SelectorTransformation))
  }
}
