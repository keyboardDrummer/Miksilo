package core.grammar

import core.transformation._
import core.transformation.grammars.{GrammarCatalogue, ProgramGrammar}
import core.transformation.sillyCodePieces.GrammarTransformation

class TestGrammarUtils {

  val manager: TransformationsToPackrat = new TransformationsToPackrat()

  def buildParser(transformations: Seq[GrammarTransformation], key: Any): String => manager.ParseResult[Any] = {

    object SelectorTransformation extends GrammarTransformation {
      override def transformGrammars(grammars: GrammarCatalogue) {
        grammars.find(ProgramGrammar).inner = grammars.find(key).inner
      }

      override def dependencies: Set[Contract] = Set.empty
    }

    manager.buildParser(transformations ++ Seq(SelectorTransformation))
  }
}
