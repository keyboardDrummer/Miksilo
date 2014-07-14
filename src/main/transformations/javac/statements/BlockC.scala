package transformations.javac.statements

import core.transformation._

object BlockC extends GrammarTransformation {


  override def dependencies: Set[Contract] = Set(StatementC)

  object BlockGrammar

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val statement = grammars.find(StatementC.StatementGrammar)
    grammars.create(BlockGrammar, "{" ~> (statement *) <~ "}")
  }
}
