package miksilo.modularLanguages.deltas.verilog

import miksilo.modularLanguages.core.deltas.{Contract, DeltaWithGrammar}
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.languageServer.core.language.Language
import miksilo.modularLanguages.core.node.{Node, NodeField}
import miksilo.modularLanguages.deltas.statement.BlockDelta.BlockOrStatementGrammar
import miksilo.modularLanguages.deltas.statement.{BlockDelta, StatementDelta}

object BeginEndDelta extends DeltaWithGrammar {
  object Label extends NodeField

  def neww(statements: Seq[Node], label: Option[String] = None): Node =
    BlockDelta.Shape.create(BlockDelta.Statements -> statements, Label -> label)

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val statement = find(StatementDelta.Grammar)
    val block = find(BlockDelta.BlockGrammar)
    val label = (":" ~~< identifier).option.as(Label)
    val beginEnd = "begin" ~ label ~ (statement ~< ";").manyVertical.as(BlockDelta.Statements) ~ "end" asNode BlockDelta.Shape
    block.inner = beginEnd

    statement.addAlternative(block)
    find(BlockOrStatementGrammar).inner = statement
  }

  override def description: String = "Adds the begin-end block"

  override def dependencies: Set[Contract] = Set(BlockDelta)
}
