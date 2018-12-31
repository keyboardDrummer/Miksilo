package deltas.javac.methods.assignment

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.language.node._
import core.deltas.path.{NodePath, PathRoot}
import core.language.{Compilation, Language}
import deltas.javac.expressions.additive.AdditionDelta

object IncrementAssignmentDelta extends DeltaWithPhase with DeltaWithGrammar {

  override def description: String = "Defines the += operator."

  override def dependencies: Set[Contract] = Set(AdditionDelta, AssignmentDelta)

  def incrementAssignment(target: Node, value: Node) =
    new Node(Shape, AssignmentDelta.Target -> target, AssignmentDelta.Value -> value)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val assignmentGrammar = find(AssignmentPrecedence.AssignmentGrammar)
    val assignmentTarget = find(AssignmentDelta.AssignmentTargetGrammar)
    val incrementAssignmentGrammar = assignmentTarget.as(AssignmentDelta.Target) ~~
      ("+=" ~~> assignmentGrammar.as(AssignmentDelta.Value)) asNode Shape
    assignmentGrammar.addAlternative(incrementAssignmentGrammar)
  }

  def transformIncrementAssignment(incrementAssignment: NodePath, state: Language): Unit = {
    val target = getTarget(incrementAssignment)
    val newValue = AdditionDelta.Shape.createWithSource(
      AdditionDelta.Left -> incrementAssignment.current(AssignmentDelta.Target),
      AdditionDelta.Right -> incrementAssignment.getWithSource(AssignmentDelta.Value))
    val assignment = AssignmentDelta.neww(target, newValue)
    incrementAssignment.replaceData(assignment)
  }

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    PathRoot(program).visitShape(Shape, obj => transformIncrementAssignment(obj, compilation))
  }

  object Shape extends NodeShape

  def getValue[T <: NodeLike](incrementAssignment: T): T = {
    incrementAssignment(AssignmentDelta.Value).asInstanceOf[T]
  }

  def getTarget[T <: NodeLike](incrementAssignment: T): T = {
    incrementAssignment(AssignmentDelta.Target).asInstanceOf[T]
  }
}
