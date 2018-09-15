package deltas.verilog

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.{NodeField, NodeShape}
import deltas.statement.{BlockDelta, StatementDelta}

object BeginEndDelta extends DeltaWithGrammar {
  object Shape extends NodeShape
  object Statements extends NodeField
  object Label extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val statement = find(StatementDelta.Grammar)
    val block = find(BlockDelta.Grammar)
    val label = (":" ~~< identifier).option.as(Label)
    val beginEnd = "begin" ~ label ~ (statement ~< ";").manyVertical.as(Statements) ~ "end"
    block.inner = beginEnd

    findPath(BlockDelta.BlockOrStatementGrammar, BlockDelta.StatementAsBlockGrammar).removeMeFromOption() //Turn BlockOrStatement into just Block
  }

  override def description: String = "Adds the begin-end block"
}
