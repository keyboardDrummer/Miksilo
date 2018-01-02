package deltas.javac.statements

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.GrammarKey

object BlockDelta extends DeltaWithGrammar {

  override def dependencies: Set[Contract] = Set(StatementSkeleton)

  val indentAmount = 4
  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val statementGrammar = find(StatementSkeleton.StatementGrammar)
    val blockGrammar = create(Grammar, "{" %> statementGrammar.manyVertical.indent(indentAmount) %< "}")
    val statementAsBlockGrammar = create(StatementAsBlockGrammar, statementGrammar.map[Any, Seq[Any]](statement => Seq(statement), x => x.head))
    create(BlockOrStatementGrammar, blockGrammar | statementAsBlockGrammar)
  }

  object BlockOrStatementGrammar extends GrammarKey
  object StatementAsBlockGrammar extends GrammarKey
  object Grammar extends GrammarKey

  override def description: String = "Defines a grammar for blocks."
}
