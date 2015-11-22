package transformations.bytecode.extraBooleanInstructions

import core.particles.node.Node
import core.particles.{CompilationState, Contract, ParticleWithPhase}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.additions.LabelledTargets
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.coreInstructions.integers.integerCompare.IfIntegerCompareNotEqualC
import transformations.bytecode.coreInstructions.integers.integerCompare.IfNotZero.IfNotZeroKey
import transformations.bytecode.coreInstructions.integers.integerCompare.IfZeroC.IfZeroKey
import transformations.bytecode.extraBooleanInstructions.IntegerEqualsInstructionC.IntegerEqualsInstructionKey
import transformations.bytecode.extraBooleanInstructions.LessThanInstructionC.LessThanInstructionKey
import transformations.bytecode.extraBooleanInstructions.NotInstructionC.NotInstructionKey

import scala.collection.mutable

object OptimizeComparisonInstructionsC extends ParticleWithPhase {

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton, LessThanInstructionC, IfIntegerCompareNotEqualC,
    NotInstructionC, IntegerEqualsInstructionC)

  override def transform(program: Node, state: CompilationState): Unit = {

    val clazz = program
    val codeAnnotations: Seq[Node] = CodeAttribute.getCodeAnnotations(clazz)

    for (codeAnnotation <- codeAnnotations) {
      processCodeAnnotation(codeAnnotation)
    }

    def processCodeAnnotation(codeAnnotation: Node): Option[Any] = {
      val instructions = CodeAttribute.getCodeInstructions(codeAnnotation)
      val newInstructions: Seq[Node] = getNewInstructions(instructions)
      codeAnnotation(CodeAttribute.CodeInstructionsKey) = newInstructions
    }

    def getNewInstructions(instructions: Seq[Node]) = {

      var newInstructions = mutable.ArrayBuffer[Node]()

      var i = 0
      while (i < instructions.size - 1) {
        val first = instructions(i)
        val second = instructions(i + 1)

        val replacementInstruction : Option[Node] = first.clazz match {
          case LessThanInstructionKey => findLessThanReplacement(second)
          case NotInstructionKey => findNotReplacement(second)
          case IntegerEqualsInstructionKey => findIntegerEqualsReplacement(second)
          case _ => None
        }
        if (replacementInstruction.isDefined)
          i += 1

        newInstructions += replacementInstruction.fold(first)(x => x)
        i += 1
      }
      val lastInstructionWasNotReplaced = i == instructions.size - 1
      if (lastInstructionWasNotReplaced)
        newInstructions += instructions(i)

      newInstructions
    }
  }

  def findIntegerEqualsReplacement(second: Node): Option[Node] = {
    second.clazz match {
      case IfZeroKey =>
        val target = LabelledTargets.getJumpInstructionLabel(second)
        Some(LabelledTargets.ifIntegerCompareNotEquals(target))
      case IfNotZeroKey =>
        val target = LabelledTargets.getJumpInstructionLabel(second)
        Some(LabelledTargets.ifIntegerCompareEquals(target))
      case _ => None
    }
  }

  def findNotReplacement(second: Node): Option[Node] = {
    second.clazz match {
      case IfZeroKey =>
        val target = LabelledTargets.getJumpInstructionLabel(second)
        Some(LabelledTargets.ifNotZero(target))
      case IfNotZeroKey =>
        val target = LabelledTargets.getJumpInstructionLabel(second)
        Some(LabelledTargets.ifZero(target))
      case _ => None
    }
  }

  def findLessThanReplacement(second: Node): Option[Node] = {
    second.clazz match {
      case IfZeroKey =>
        val target = LabelledTargets.getJumpInstructionLabel(second)
        Some(LabelledTargets.ifIntegerCompareGreaterEquals(target))
      case IfNotZeroKey =>
        val target = LabelledTargets.getJumpInstructionLabel(second)
        Some(LabelledTargets.ifIntegerCompareLess(target))
      case _ => None
    }
  }

  override def description: String = "Combines instructions to reduce the instruction count. " +
    "Applies mostly to custom instructions related to comparison operations, followed by a jumps."
}
