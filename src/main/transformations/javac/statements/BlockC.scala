package transformations.javac.statements

import core.document.Empty
import core.transformation._
import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation

object BlockC extends GrammarTransformation {

  override def dependencies: Set[Contract] = Set(StatementC)

  val indentAmount = 2
  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val statement = grammars.find(StatementC.StatementGrammar)
    grammars.create(BlockGrammar, "{" %> statement.someSeparatedVertical(Empty).indent(indentAmount) %< "}")
  }

  object BlockGrammar

}
