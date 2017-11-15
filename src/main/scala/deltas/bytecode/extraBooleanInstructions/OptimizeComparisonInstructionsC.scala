package deltas.bytecode.extraBooleanInstructions

import core.particles.node.Node
import core.particles.{Compilation, Contract, DeltaWithPhase}
import deltas.bytecode.ByteCodeSkeleton
import deltas.bytecode.additions.LabelledLocations
import deltas.bytecode.attributes.CodeAttribute
import deltas.bytecode.attributes.CodeAttribute.CodeWrapper
import deltas.bytecode.coreInstructions.integers.integerCompare.{IfIntegerCompareNotEqualDelta, IfNotZero, IfZeroDelta}
import deltas.bytecode.extraBooleanInstructions.GreaterThanInstructionC.GreaterThanInstructionKey
import deltas.bytecode.extraBooleanInstructions.IntegerEqualsInstructionC.IntegerEqualsInstructionKey
import deltas.bytecode.extraBooleanInstructions.LessThanInstructionC.LessThanInstructionKey
import deltas.bytecode.extraBooleanInstructions.NotInstructionC.NotInstructionKey

import scala.collection.mutable

object OptimizeComparisonInstructionsC extends DeltaWithPhase {

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton, LessThanInstructionC, IfIntegerCompareNotEqualDelta,
    NotInstructionC, IntegerEqualsInstructionC)

  override def transform(program: Node, state: Compilation): Unit = {

    val clazz = program
    val codeAnnotations: Seq[Node] = CodeAttribute.getCodeAnnotations(clazz)

    for (codeAnnotation <- codeAnnotations) {
      processCodeAnnotation(codeAnnotation)
    }

    def processCodeAnnotation(codeAnnotation: CodeWrapper[Node]): Unit = {
      val instructions = codeAnnotation.instructions
      val newInstructions: Seq[Node] = getNewInstructions(instructions)
      codeAnnotation.instructions = newInstructions
    }

    def getNewInstructions(instructions: Seq[Node]) = {

      var newInstructions = mutable.ArrayBuffer[Node]()

      var i = 0
      while (i < instructions.size - 1) {
        val first = instructions(i)
        val second = instructions(i + 1)

        val replacementInstruction : Option[Node] = first.clazz match {
          case LessThanInstructionKey => findLessThanReplacement(second)
          case GreaterThanInstructionKey => findGreaterThanReplacement(second)
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
      case IfZeroDelta.Clazz =>
        val target = LabelledLocations.getJumpInstructionLabel(second)
        Some(LabelledLocations.ifIntegerCompareNotEquals(target))
      case IfNotZero.Clazz =>
        val target = LabelledLocations.getJumpInstructionLabel(second)
        Some(LabelledLocations.ifIntegerCompareEquals(target))
      case _ => None
    }
  }

  def findNotReplacement(second: Node): Option[Node] = {
    second.clazz match {
      case IfZeroDelta.Clazz =>
        val target = LabelledLocations.getJumpInstructionLabel(second)
        Some(LabelledLocations.ifNotZero(target))
      case IfNotZero.Clazz =>
        val target = LabelledLocations.getJumpInstructionLabel(second)
        Some(LabelledLocations.ifZero(target))
      case _ => None
    }
  }

  def findGreaterThanReplacement(second: Node): Option[Node] = {
    second.clazz match {
      case IfZeroDelta.Clazz =>
        val target = LabelledLocations.getJumpInstructionLabel(second)
        Some(LabelledLocations.ifIntegerCompareLessEquals(target))
      case IfNotZero.Clazz =>
        val target = LabelledLocations.getJumpInstructionLabel(second)
        Some(LabelledLocations.ifIntegerCompareGreater(target))
      case _ => None
    }
  }

  def findLessThanReplacement(second: Node): Option[Node] = {
    second.clazz match {
      case IfZeroDelta.Clazz =>
        val target = LabelledLocations.getJumpInstructionLabel(second)
        Some(LabelledLocations.ifIntegerCompareGreaterEquals(target))
      case IfNotZero.Clazz =>
        val target = LabelledLocations.getJumpInstructionLabel(second)
        Some(LabelledLocations.ifIntegerCompareLess(target))
      case _ => None
    }
  }

  override def description: String = "Combines instructions to reduce the instruction count. " +
    "Applies mostly to custom instructions related to comparison operations, followed by a jumps."
}
