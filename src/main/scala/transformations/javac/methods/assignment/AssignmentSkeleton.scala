package transformations.javac.methods.assignment

import core.bigrammar.BiFailure
import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node, NodeLike}
import core.particles.path.Path
import transformations.bytecode.coreInstructions.integers.StoreIntegerDelta$
import transformations.bytecode.coreInstructions.objects.StoreAddressDelta$
import transformations.bytecode.coreInstructions.{Duplicate2InstructionDelta$, DuplicateInstructionDelta$}
import transformations.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import transformations.javac.methods.MethodC
import transformations.bytecode.types.TypeSkeleton

object AssignmentSkeleton extends ExpressionInstance with WithState {

  def getAssignmentTarget[T <: NodeLike](assignment: T) = assignment(AssignmentTarget).asInstanceOf[T]

  def getAssignmentValue[T <: NodeLike](assignment: T) = assignment(AssignmentValue).asInstanceOf[T]

  override def dependencies: Set[Contract] = Set(MethodC, StoreAddressDelta$, StoreIntegerDelta$, AssignmentPrecedence)

  override def transformGrammars(grammars: GrammarCatalogue, state: CompilationState): Unit = {
    val targetGrammar = grammars.create(AssignmentTargetGrammar, BiFailure())
    val expressionGrammar = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    val assignmentInner = (targetGrammar <~~ "=") ~~ expressionGrammar
    val assignmentGrammar = nodeGrammar(assignmentInner, AssignmentKey, AssignmentTarget, AssignmentValue)
    expressionGrammar.addOption(assignmentGrammar)
  }

  object AssignmentTargetGrammar

  def assignment(target: Node, value: Node) = new Node(AssignmentKey, AssignmentTarget -> target, AssignmentValue -> value)

  object AssignmentKey extends Key

  object AssignmentTarget extends Key

  object AssignmentValue extends Key

  override val key: Key = AssignmentKey

  override def getType(assignment: Path, state: CompilationState): Node = {
    val target = getAssignmentTarget(assignment)
    ExpressionSkeleton.getType(state)(target)
  }

  def createState = new State()
  class State {
    val assignFromStackByteCodeRegistry = new ClassRegistry[Path => Seq[Node]]
  }

  override def toByteCode(assignment: Path, state: CompilationState): Seq[Node] = {
    val value = getAssignmentValue(assignment)
    val valueInstructions = ExpressionSkeleton.getToInstructions(state)(value)
    val target = getAssignmentTarget(assignment)
    val assignInstructions = getState(state).assignFromStackByteCodeRegistry(target.clazz)(target)
    val valueType = ExpressionSkeleton.getType(state)(value)
    val duplicateInstruction = TypeSkeleton.getTypeSize(valueType, state) match
    {
      case 1 => DuplicateInstructionDelta$.duplicate
      case 2 =>  Duplicate2InstructionDelta$.duplicate
    }
    valueInstructions ++ Seq(duplicateInstruction) ++ assignInstructions
  }

  override def description: String = "Enables assignment to an abstract target using the = operator."
}
