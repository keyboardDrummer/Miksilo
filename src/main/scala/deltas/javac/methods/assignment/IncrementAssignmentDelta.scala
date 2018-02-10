package deltas.javac.methods.assignment

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.node._
import core.deltas.path.{NodePath, PathRoot}
import core.language.Language
import deltas.javac.expressions.additive.AdditionDelta

object IncrementAssignmentDelta extends DeltaWithPhase with DeltaWithGrammar {

  override def description: String = "Defines the += operator."

  override def dependencies: Set[Contract] = Set(AdditionDelta, AssignmentSkeleton)

  def incrementAssignment(target: Node, value: Node) =
    new Node(IncrementAssignmentKey, TargetKey -> target, ValueKey -> value)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val assignmentGrammar = find(AssignmentPrecedence.AssignmentGrammar)
    val assignmentTarget = find(AssignmentSkeleton.AssignmentTargetGrammar)
    val incrementAssignmentGrammar = assignmentTarget.as(TargetKey) ~~ ("+=" ~~> assignmentGrammar.as(ValueKey)) asNode IncrementAssignmentKey
    assignmentGrammar.addOption(incrementAssignmentGrammar)
  }

  def transformIncrementAssignment(incrementAssignment: NodePath, state: Language): Unit = {
    val target = getTarget(incrementAssignment)
    val value = getValue(incrementAssignment)
    val newValue = AdditionDelta.addition(value, target)
    val assignment = AssignmentSkeleton.assignment(target, newValue)
    incrementAssignment.replaceWith(assignment)
  }

  override def transformProgram(program: Node, state: Compilation): Unit = {
    PathRoot(program).visit(obj => obj.shape match {
      case IncrementAssignmentKey => transformIncrementAssignment(obj, state)
      case _ =>
    })
  }

  object IncrementAssignmentKey extends NodeShape

  object TargetKey extends NodeField

  object ValueKey extends NodeField

  def getValue[T <: NodeLike](incrementAssignment: T): T = {
    incrementAssignment(ValueKey).asInstanceOf[T]
  }

  def getTarget[T <: NodeLike](incrementAssignment: T): T = {
    incrementAssignment(TargetKey).asInstanceOf[T]
  }
}
