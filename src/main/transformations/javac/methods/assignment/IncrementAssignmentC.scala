package transformations.javac.methods.assignment

import core.particles.grammars.GrammarCatalogue
import core.particles.{Contract, MetaObject, CompilationState}
import transformations.javac.expressions.additive.AdditionC
import transformations.javac.expressions.{ExpressionSkeleton, ExpressionInstance}
import transformations.types.IntTypeC

object IncrementAssignmentC extends ExpressionInstance {

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

  override val key: AnyRef = IncrementAssignmentKey

  override def getType(expression: MetaObject, state: CompilationState): MetaObject = IntTypeC.intType

  override def toByteCode(incrementAssignment: MetaObject, state: CompilationState): Seq[MetaObject] = {
    val target = incrementAssignment(TargetKey).asInstanceOf[MetaObject]
    val value = incrementAssignment(ValueKey).asInstanceOf[MetaObject]
    val newValue = AdditionC.addition(value, target)
    val assignment = AssignmentSkeleton.assignment(target, newValue)

    val toInstructions = ExpressionSkeleton.getToInstructions(state)
    toInstructions(assignment)
  }

  override def description: String = "Defines the += operator."
}
