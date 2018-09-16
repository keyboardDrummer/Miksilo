package deltas.verilog

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.{Node, NodeField, NodeShape}
import deltas.statement.BlockDelta.BlockOrStatementGrammar
import deltas.statement.{BlockDelta, StatementDelta}

object BeginEndDelta extends DeltaWithGrammar { //TODO re-use BlockAsStatementDelta
  object Shape extends NodeShape
  object Statements extends NodeField
  object Label extends NodeField

  def neww(statements: Seq[Node], label: Option[String] = None): Node =
    Shape.create(Statements -> statements, Label -> label)

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val statement = find(StatementDelta.Grammar)
    val block = find(BlockDelta.Grammar)
    val label = (":" ~~< identifier).option.as(Label)
    val beginEnd = "begin" ~ label ~ (statement ~< ";").manyVertical.as(Statements) ~ "end" asNode Shape
    block.inner = beginEnd

    statement.addAlternative(block)
    find(BlockOrStatementGrammar).inner = statement
//    find(BlockDelta.StatementAsBlockGrammar).inner =
//      (ValueGrammar(None).as(Label) ~
//      statement.map[Any, Seq[Any]](statement => Seq(statement), x => x.head).as(Statements)) asNode Shape
  }

  override def description: String = "Adds the begin-end block"
}
