package deltas.javac.methods.assignment

import core.bigrammar.grammars.BiFailure
import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node._
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.expression.ExpressionDelta
import deltas.javac.expressions.ExpressionInstance

object SimpleAssignmentDelta extends DeltaWithGrammar with ExpressionInstance {

  def getTarget[T <: NodeLike](assignment: T): T = assignment(Target).asInstanceOf[T]

  def getValue[T <: NodeLike](assignment: T): T = assignment(Value).asInstanceOf[T]

  override def dependencies: Set[Contract] = Set(AssignmentPrecedence)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val targetGrammar = create(AssignmentTargetGrammar, BiFailure())
    val expressionGrammar = find(ExpressionDelta.FirstPrecedenceGrammar) //TODO shouldn't this use AssignmentPrecedence?
    val assignmentGrammar = targetGrammar.as(Target) ~~< "=" ~~ expressionGrammar.as(Value) asNode Shape
    expressionGrammar.addAlternative(assignmentGrammar)
  }

  object AssignmentTargetGrammar extends GrammarKey

  def neww(target: Node, value: Node) = new Node(Shape, Target -> target, Value -> value)

  object Shape extends NodeShape

  object Target extends NodeField

  object Value extends NodeField

  override val shape = Shape

  override def getType(assignment: NodePath, compilation: Compilation): Node = {
    val target = getTarget(assignment)
    ExpressionDelta.getType(compilation)(target)
  }

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
