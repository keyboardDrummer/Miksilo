package deltas.javac.methods.assignment

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.language.node._
import core.deltas.path.{NodePath, PathRoot}
import core.language.{Compilation, Language}
import deltas.javac.expressions.additive.AdditionDelta

object IncrementAssignmentDelta extends DeltaWithPhase with DeltaWithGrammar {

  override def description: String = "Defines the += operator."

  override def dependencies: Set[Contract] = Set(AdditionDelta, AssignmentSkeleton)

  def incrementAssignment(target: Node, value: Node) =
    new Node(IncrementAssignmentKey, AssignmentSkeleton.Target -> target, AssignmentSkeleton.Value -> value)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val assignmentGrammar = find(AssignmentPrecedence.AssignmentGrammar)
    val assignmentTarget = find(AssignmentSkeleton.AssignmentTargetGrammar)
    val incrementAssignmentGrammar = assignmentTarget.as(AssignmentSkeleton.Target) ~~
      ("+=" ~~> assignmentGrammar.as(AssignmentSkeleton.Value)) asNode IncrementAssignmentKey
    assignmentGrammar.addAlternative(incrementAssignmentGrammar)
  }

  def transformIncrementAssignment(incrementAssignment: NodePath, state: Language): Unit = {
    val target = getTarget(incrementAssignment)
    val value = getValue(incrementAssignment)
    val newValue = AdditionDelta.Shape.createWithSource(
      AdditionDelta.Left -> incrementAssignment.current(AssignmentSkeleton.Target),
      AdditionDelta.Right -> incrementAssignment.getWithSource(AssignmentSkeleton.Value))
    val assignment = AssignmentSkeleton.assignment(target, newValue)
    incrementAssignment.replaceWith(assignment)
  }

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    PathRoot(program).visitShape(IncrementAssignmentKey, obj => transformIncrementAssignment(obj, compilation))
  }

  object IncrementAssignmentKey extends NodeShape

  def getValue[T <: NodeLike](incrementAssignment: T): T = {
    incrementAssignment(AssignmentSkeleton.Value).asInstanceOf[T]
  }

  def getTarget[T <: NodeLike](incrementAssignment: T): T = {
    incrementAssignment(AssignmentSkeleton.Target).asInstanceOf[T]
  }
}
