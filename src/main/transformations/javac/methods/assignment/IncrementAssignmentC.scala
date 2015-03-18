package transformations.javac.methods.assignment

import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{MetaObject, MetaLike}
import core.particles.path.{Path, Root}
import transformations.javac.expressions.additive.AdditionC

//TODO refactor so it uses a phase to reduce itself.
object IncrementAssignmentC extends ParticleWithPhase with ParticleWithGrammar {

  override def dependencies: Set[Contract] = Set(AdditionC, AssignmentSkeleton)

  def incrementAssignment(target: MetaObject, value: MetaObject) =
    new MetaObject(IncrementAssignmentKey, TargetKey -> target, ValueKey -> value)

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val assignmentGrammar = grammars.find(AssignmentPrecedence.AssignmentGrammar)
    val assignmentTarget = grammars.find(AssignmentSkeleton.AssignmentTargetGrammar)
    val incrementAssignmentGrammar = assignmentTarget ~~ ("+=" ~~> assignmentGrammar) ^^ parseMap(IncrementAssignmentKey, TargetKey, ValueKey)
    assignmentGrammar.addOption(incrementAssignmentGrammar)
  }

  object IncrementAssignmentKey

  object TargetKey

  object ValueKey

  def transformIncrementAssignment(incrementAssignment: Path, state: CompilationState): Unit = {
    val target = getTarget(incrementAssignment)
    val value = getValue(incrementAssignment)
    val newValue = AdditionC.addition(value, target)
    val assignment = AssignmentSkeleton.assignment(target, newValue)
    incrementAssignment.replaceWith(assignment)
  }

  override def transform(program: MetaObject, state: CompilationState): Unit = {
    new Root(program).transform[Path](obj => obj.clazz match {
      case IncrementAssignmentKey => transformIncrementAssignment(obj, state)
      case _ =>
    })
  }

  def getValue[T <: MetaLike](incrementAssignment: T): T = {
    incrementAssignment(ValueKey).asInstanceOf[T]
  }

  def getTarget[T <: MetaLike](incrementAssignment: T): T = {
    incrementAssignment(TargetKey).asInstanceOf[T]
  }

  override def description: String = "Defines the += operator."
}
