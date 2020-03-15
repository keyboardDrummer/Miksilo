package miksilo.modularLanguages.deltas.statement.assignment

import miksilo.modularLanguages.core.bigrammar.grammars.BiFailure
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.modularLanguages.core.deltas.{Contract, DeltaWithGrammar}
import miksilo.modularLanguages.core.node._
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.Type
import miksilo.modularLanguages.deltas.expression.{ExpressionDelta, ExpressionInstance}

object SimpleAssignmentDelta extends DeltaWithGrammar with ExpressionInstance {

  def getTarget[T <: NodeLike](assignment: T): T = assignment(Target).asInstanceOf[T]

  def getValue[T <: NodeLike](assignment: T): T = assignment(Value).asInstanceOf[T]

  override def dependencies: Set[Contract] = Set(AssignmentPrecedence)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val targetGrammar = create(AssignmentTargetGrammar, BiFailure())
    val expressionGrammar = find(ExpressionDelta.FirstPrecedenceGrammar) //TODO shouldn't this use AssignmentPrecedence?
    val assignmentGrammar = create(Target, targetGrammar.as(Target)) ~~< "=" ~~ expressionGrammar.as(Value) asLabelledNode Shape
    expressionGrammar.addAlternative(assignmentGrammar)
  }

  object AssignmentTargetGrammar extends GrammarKey

  def neww(target: Node, value: Node) = new Node(Shape, Target -> target, Value -> value)

  object Shape extends NodeShape

  object Target extends NodeField

  object Value extends NodeField

  override val shape = Shape

  override def description: String = "Enables assignment to an abstract target using the = operator."

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, assignment: NodePath, _type: Type, parentScope: Scope): Unit = {
    val value = getValue(assignment)
    val valueType = ExpressionDelta.getType(compilation, builder, value, parentScope)
    val target = getTarget(assignment)
    val targetType = ExpressionDelta.getType(compilation, builder, target, parentScope)
    builder.typesAreEqual(targetType, _type)
    builder.isFirstSubsetOfSecond(valueType, targetType)
  }
}
