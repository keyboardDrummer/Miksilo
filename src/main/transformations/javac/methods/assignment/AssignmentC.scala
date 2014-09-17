package transformations.javac.methods.assignment

import core.grammar._
import core.transformation._
import core.transformation.grammars.GrammarCatalogue
import transformations.bytecode.coreInstructions.integers.StoreIntegerC
import transformations.bytecode.coreInstructions.objects.StoreAddressC
import transformations.javac.expressions.{ExpressionC, ExpressionInstance}
import transformations.javac.methods.MethodC

import scala.collection.mutable

object AssignmentC extends ExpressionInstance {

  def getAssignmentTarget(assignment: MetaObject) = assignment(AssignmentTarget).asInstanceOf[MetaObject]

  def getAssignmentValue(assignment: MetaObject) = assignment(AssignmentValue).asInstanceOf[MetaObject]

  override def dependencies: Set[Contract] = Set(MethodC, StoreAddressC, StoreIntegerC, AssignmentPrecedence)

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val targetGrammar = grammars.create(AssignmentTargetGrammar, FailureG)
    val expressionGrammar = grammars.find(ExpressionC.ExpressionGrammar)
    val assignmentGrammar = (targetGrammar <~~ "=") ~~ expressionGrammar ^^ parseMap(AssignmentKey, AssignmentTarget, AssignmentValue)
    expressionGrammar.addOption(assignmentGrammar)
  }

  object AssignmentTargetGrammar

  def assignment(target: MetaObject, value: MetaObject) = new MetaObject(AssignmentKey, AssignmentTarget -> target, AssignmentValue -> value)

  object AssignmentKey

  object AssignmentTarget

  object AssignmentValue

  override val key: AnyRef = AssignmentKey

  override def getType(assignment: MetaObject, state: TransformationState): MetaObject = {
    val target = getAssignmentTarget(assignment)
    ExpressionC.getType(state)(target)
  }

  def getState(state: TransformationState) = state.data.getOrElseUpdate(this, new State()).asInstanceOf[State]

  class State {
    val assignFromStackByteCodeRegistry = new mutable.HashMap[Any, MetaObject => Seq[MetaObject]]
  }

  override def toByteCode(assignment: MetaObject, state: TransformationState): Seq[MetaObject] = {
    val value = getAssignmentValue(assignment)
    val valueInstructions = ExpressionC.getToInstructions(state)(value)
    val target = getAssignmentTarget(assignment)
    val assignInstructions = getState(state).assignFromStackByteCodeRegistry(target.clazz)(target)
    valueInstructions ++ assignInstructions

  }
}
