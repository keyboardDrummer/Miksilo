package transformations.javac.methods.assignment

import core.grammar._
import core.transformation.grammars.GrammarCatalogue
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.javac.expressions.additive.AdditionC
import transformations.javac.expressions.{ExpressionC, ExpressionInstance}
import transformations.types.IntTypeC

object IncrementAssignmentC extends ExpressionInstance {

  override def dependencies: Set[Contract] = Set(AdditionC, AssignmentC)

  def incrementAssignment(target: MetaObject, value: MetaObject) =
    new MetaObject(IncrementAssignmentKey, TargetKey -> target, ValueKey -> value)

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val assignmentGrammar = grammars.find(AssignmentPrecedence.AssignmentGrammar)
    val assignmentTarget = grammars.find(AssignmentC.AssignmentTargetGrammar)
    val incrementAssignmentGrammar: Grammar = assignmentTarget ~ ("+=" ~> assignmentGrammar) ^^
      { case target ~ value => new MetaObject(IncrementAssignmentKey, TargetKey -> target, ValueKey -> value) }
    assignmentGrammar.orToInner(incrementAssignmentGrammar)
  }

  object IncrementAssignmentKey
  object TargetKey
  object ValueKey

  override val key: AnyRef = IncrementAssignmentKey

  override def getType(expression: MetaObject, state: TransformationState): MetaObject = IntTypeC.intType

  override def toByteCode(incrementAssignment: MetaObject, state: TransformationState): Seq[MetaObject] = {
    val target = incrementAssignment(TargetKey).asInstanceOf[MetaObject]
    val value = incrementAssignment(ValueKey).asInstanceOf[MetaObject]
    val newValue = AdditionC.addition(value, target)
    val assignment = AssignmentC.assignment(target, newValue)

    val toInstructions = ExpressionC.getToInstructions(state)
    toInstructions(assignment)
  }
}
