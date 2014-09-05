package transformations.javac.statements

import core.transformation._
import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation

object BlockC extends GrammarTransformation {


  override def dependencies: Set[Contract] = Set(StatementC)

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val statement = grammars.find(StatementC.StatementGrammar)
    grammars.create(BlockGrammar, "{" %> (statement *).indent(4) %< "}")
  }

  object BlockGrammar

}
