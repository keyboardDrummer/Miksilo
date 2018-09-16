package deltas.verilog

import core.bigrammar.BiGrammar
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.{Node, NodeField, NodeShape}
import deltas.expressions.ExpressionDelta
import deltas.statement.StatementDelta

object NonBlockingAssignmentDelta extends DeltaWithGrammar { //TODO merge with AssignmentSkeleton
  object Shape extends NodeShape
  object Target extends NodeField
  object Value extends NodeField

  def neww(target: String, value: Node): Node = Shape.create(Target -> target, Value -> value)

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val expression = find(ExpressionDelta.FirstPrecedenceGrammar)
    val statement = find(StatementDelta.Grammar)
    val assignment: BiGrammar = identifier.as(Target) ~~ "<=" ~~ expression.as(Value) asNode Shape
    statement.addAlternative(assignment)
  }

  override def description: String = "Adds the non-blocking assignment operator <="
}
