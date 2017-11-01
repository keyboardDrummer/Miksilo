package transformations.javac.statements

import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.node.GrammarKey

object BlockC extends DeltaWithGrammar {

  override def dependencies: Set[Contract] = Set(StatementSkeleton)

  val indentAmount = 4
  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    import grammars._
    val statementGrammar = find(StatementSkeleton.StatementGrammar)
    val blockGrammar = create(BlockGrammar, "{" %> statementGrammar.manyVertical.indent(indentAmount) %< "}")
    val statementAsBlockGrammar = create(StatementAsBlockGrammar, statementGrammar ^^(statement => Seq(statement), x => Some(x.asInstanceOf[Seq[Any]].head)))
    create(BlockOrStatementGrammar, blockGrammar | statementAsBlockGrammar)
  }

  object BlockOrStatementGrammar extends GrammarKey
  object StatementAsBlockGrammar extends GrammarKey
  object BlockGrammar extends GrammarKey

  override def description: String = "Defines a grammar for blocks."
}
