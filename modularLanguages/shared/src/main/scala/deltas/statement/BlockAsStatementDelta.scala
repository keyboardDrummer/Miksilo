package deltas.statement

import core.deltas.grammars.LanguageGrammars
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.Language

object BlockAsStatementDelta extends DeltaWithGrammar {
  override def description: String = "Allows using a block as a statement, to create a new scope."

  override def dependencies: Set[Contract] = Set(BlockDelta)

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val statementGrammar = find(StatementDelta.Grammar)
    val blockGrammar = find(BlockDelta.BlockGrammar)
    statementGrammar.addAlternative(blockGrammar)
  }
}
