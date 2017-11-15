package deltas.javac.methods.assignment

import core.bigrammar.grammars.BiFailure
import core.particles._
import core.particles.grammars.LanguageGrammars
import core.particles.node._
import core.particles.path.Path
import deltas.bytecode.coreInstructions.integers.StoreIntegerDelta
import deltas.bytecode.coreInstructions.objects.StoreAddressDelta
import deltas.bytecode.coreInstructions.{Duplicate2InstructionDelta, DuplicateInstructionDelta}
import deltas.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import deltas.javac.methods.MethodDelta
import deltas.bytecode.types.TypeSkeleton

object AssignmentSkeleton extends ExpressionInstance with WithLanguageRegistry {

  def getAssignmentTarget[T <: NodeLike](assignment: T) = assignment(AssignmentTarget).asInstanceOf[T]

  def getAssignmentValue[T <: NodeLike](assignment: T) = assignment(AssignmentValue).asInstanceOf[T]

  override def dependencies: Set[Contract] = Set(MethodDelta, StoreAddressDelta, StoreIntegerDelta, AssignmentPrecedence)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val targetGrammar = create(AssignmentTargetGrammar, BiFailure())
    val expressionGrammar = find(ExpressionSkeleton.ExpressionGrammar)
    val assignmentGrammar = targetGrammar.as(AssignmentTarget) ~~< "=" ~~ expressionGrammar.as(AssignmentValue) asNode AssignmentKey
    expressionGrammar.addOption(assignmentGrammar)
  }

  object AssignmentTargetGrammar extends GrammarKey

  def assignment(target: Node, value: Node) = new Node(AssignmentKey, AssignmentTarget -> target, AssignmentValue -> value)

  object AssignmentKey extends NodeClass

  object AssignmentTarget extends NodeField

  object AssignmentValue extends NodeField

  override val key = AssignmentKey

  override def getType(assignment: Path, compilation: Compilation): Node = {
    val target = getAssignmentTarget(assignment)
    ExpressionSkeleton.getType(compilation)(target)
  }

  def createRegistry = new Registry()
  class Registry {
    val assignFromStackByteCodeRegistry = new ClassRegistry[(Compilation, Path) => Seq[Node]]
  }

  override def toByteCode(assignment: Path, compilation: Compilation): Seq[Node] = {
    val value = getAssignmentValue(assignment)
    val valueInstructions = ExpressionSkeleton.getToInstructions(compilation)(value)
    val target = getAssignmentTarget(assignment)
    val assignInstructions = getRegistry(compilation).assignFromStackByteCodeRegistry(target.clazz)(compilation, target)
    val valueType = ExpressionSkeleton.getType(compilation)(value)
    val duplicateInstruction = TypeSkeleton.getTypeSize(valueType, compilation) match
    {
      case 1 => DuplicateInstructionDelta.duplicate
      case 2 =>  Duplicate2InstructionDelta.duplicate
    }
    valueInstructions ++ Seq(duplicateInstruction) ++ assignInstructions
  }

  override def description: String = "Enables assignment to an abstract target using the = operator."
}
