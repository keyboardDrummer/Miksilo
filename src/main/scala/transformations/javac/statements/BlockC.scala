package transformations.javac.statements

import core.particles._
import core.particles.grammars.GrammarCatalogue

object BlockC extends DeltaWithGrammar {

  override def dependencies: Set[Contract] = Set(StatementSkeleton)

  val indentAmount = 4
  override def transformGrammars(grammars: GrammarCatalogue, state: CompilationState): Unit = {
    val statementGrammar = grammars.find(StatementSkeleton.StatementGrammar)
    val blockGrammar = grammars.create(BlockGrammar, "{" %> statementGrammar.manyVertical.indent(indentAmount) %< "}")
    val statementAsBlockGrammar = grammars.create(StatementAsBlockGrammar, statementGrammar ^^(statement => Seq(statement), x => Some(x.asInstanceOf[Seq[Any]].head)))
    grammars.create(BlockOrStatementGrammar, blockGrammar | statementAsBlockGrammar)
  }

  object BlockOrStatementGrammar
  object StatementAsBlockGrammar
  object BlockGrammar

  override def description: String = "Defines a grammar for blocks."
}
