package transformations.javac.methods.assignment

import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.node._
import core.particles.path.{Path, PathRoot}
import transformations.javac.expressions.additive.AdditionC

//TODO refactor so it uses a phase to reduce itself.
object IncrementAssignmentC extends DeltaWithPhase with DeltaWithGrammar {

  override def dependencies: Set[Contract] = Set(AdditionC, AssignmentSkeleton)

  def incrementAssignment(target: Node, value: Node) =
    new Node(IncrementAssignmentKey, TargetKey -> target, ValueKey -> value)

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    val assignmentGrammar = grammars.find(AssignmentPrecedence.AssignmentGrammar)
    val assignmentTarget = grammars.find(AssignmentSkeleton.AssignmentTargetGrammar)
    val incrementAssignmentGrammar = assignmentTarget ~~ ("+=" ~~> assignmentGrammar) asNode(IncrementAssignmentKey, TargetKey, ValueKey)
    assignmentGrammar.addOption(incrementAssignmentGrammar)
  }

  object IncrementAssignmentKey extends NodeClass

  object TargetKey extends NodeField

  object ValueKey extends NodeField

  def transformIncrementAssignment(incrementAssignment: Path, state: Language): Unit = {
    val target = getTarget(incrementAssignment)
    val value = getValue(incrementAssignment)
    val newValue = AdditionC.addition(value, target)
    val assignment = AssignmentSkeleton.assignment(target, newValue)
    incrementAssignment.replaceWith(assignment)
  }

  override def transform(program: Node, state: Compilation): Unit = {
    new PathRoot(program).visit(obj => obj.clazz match {
      case IncrementAssignmentKey => transformIncrementAssignment(obj, state)
      case _ =>
    })
  }

  def getValue[T <: NodeLike](incrementAssignment: T): T = {
    incrementAssignment(ValueKey).asInstanceOf[T]
  }

  def getTarget[T <: NodeLike](incrementAssignment: T): T = {
    incrementAssignment(TargetKey).asInstanceOf[T]
  }

  override def description: String = "Defines the += operator."
}
