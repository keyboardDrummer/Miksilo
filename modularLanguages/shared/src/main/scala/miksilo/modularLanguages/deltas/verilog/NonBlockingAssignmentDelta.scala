package miksilo.modularLanguages.deltas.verilog

import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.deltas.{Contract, DeltaWithGrammar}
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.modularLanguages.core.node._
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.modularLanguages.deltas.expression.ExpressionDelta
import miksilo.modularLanguages.deltas.classes.HasConstraintsDelta
import miksilo.modularLanguages.deltas.statement.StatementDelta

object NonBlockingAssignmentDelta extends DeltaWithGrammar with HasConstraintsDelta { //TODO merge with AssignmentSkeleton
  object Shape extends NodeShape
  object Target extends NodeField
  object Value extends NodeField

  implicit class NonBlockingAssignment[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def target: String = node.getValue(Target).asInstanceOf[String]
    def value: T = node(Value).asInstanceOf[T]
  }

  def neww(target: String, value: Node): Node = Shape.create(Target -> target, Value -> value)

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val expression = find(ExpressionDelta.FirstPrecedenceGrammar)
    val statement = find(StatementDelta.Grammar)
    val assignment: BiGrammar = identifier.as(Target) ~~ "<=" ~~ expression.as(Value) asNode Shape
    statement.addAlternative(assignment) //TODO shouldn't this be added to expression?
  }

  override def description: String = "Adds the non-blocking assignment operator <="

  override def shape: NodeShape = Shape

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
    val assignment: NonBlockingAssignment[NodePath] = path
    ExpressionDelta.addConstraints(compilation, builder, assignment.value, builder.typeVariable(), parentScope)
    builder.resolve(assignment.target, parentScope, path.getField(Target))
  }

  override def dependencies: Set[Contract] = Set(ExpressionDelta)
}
