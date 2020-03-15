package miksilo.modularLanguages.deltas.bytecode.extraBooleanInstructions

import miksilo.modularLanguages.core.node.Node
import miksilo.modularLanguages.core.deltas.{Contract, DeltaWithPhase}
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.deltas.bytecode.ByteCodeSkeleton
import miksilo.modularLanguages.deltas.bytecode.ByteCodeSkeleton.ClassFile
import miksilo.modularLanguages.deltas.bytecode.attributes.CodeAttributeDelta
import miksilo.modularLanguages.deltas.bytecode.attributes.CodeAttributeDelta.CodeAttribute
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.integers.integerCompare.{IfIntegerCompareNotEqualDelta, IfNotZero, IfZeroDelta}
import miksilo.modularLanguages.deltas.bytecode.extraBooleanInstructions.GreaterThanInstructionDelta.GreaterThanInstructionKey
import miksilo.modularLanguages.deltas.bytecode.extraBooleanInstructions.IntegerEqualsInstructionDelta.IntegerEqualsInstructionKey
import miksilo.modularLanguages.deltas.bytecode.extraBooleanInstructions.LessThanInstructionDelta.LessThanInstructionKey
import miksilo.modularLanguages.deltas.bytecode.extraBooleanInstructions.NotInstructionDelta.NotInstructionKey
import miksilo.modularLanguages.deltas.bytecode.simpleBytecode.LabelledLocations

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object OptimizeComparisonInstructionsDelta extends DeltaWithPhase {

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton, LessThanInstructionDelta, IfIntegerCompareNotEqualDelta,
    NotInstructionDelta, IntegerEqualsInstructionDelta)

  override def transformProgram(program: Node, state: Compilation): Unit = {

    val classFile: ClassFile[Node] = program
    val codeAnnotations = CodeAttributeDelta.getCodeAnnotations(classFile)
    for (codeAnnotation <- codeAnnotations) {
      processCodeAnnotation(codeAnnotation)
    }

    def processCodeAnnotation(codeAnnotation: CodeAttribute[Node]): Unit = {
      val instructions = codeAnnotation.instructions
      val newInstructions: Seq[Node] = getNewInstructions(instructions)
      codeAnnotation.instructions = newInstructions
    }

    def getNewInstructions(instructions: Seq[Node]): Seq[Node] = {

      val newInstructions = ListBuffer.empty[Node]

      var i = 0
      while (i < instructions.size - 1) {
        val first = instructions(i)
        val second = instructions(i + 1)

        val replacementInstruction : Option[Node] = first.shape match {
          case LessThanInstructionKey => findLessThanReplacement(second)
          case GreaterThanInstructionKey => findGreaterThanReplacement(second)
          case NotInstructionKey => findNotReplacement(second)
          case IntegerEqualsInstructionKey => findIntegerEqualsReplacement(second)
          case _ => None
        }
        if (replacementInstruction.isDefined)
          i += 1

        newInstructions.addOne(replacementInstruction.fold(first)(x => x))
        i += 1
      }
      val lastInstructionWasNotReplaced = i == instructions.size - 1
      if (lastInstructionWasNotReplaced)
        newInstructions.addOne(instructions(i))

      newInstructions.toSeq
    }
  }

  def findIntegerEqualsReplacement(second: Node): Option[Node] = {
    second.shape match {
      case IfZeroDelta.`shape` =>
        val target = LabelledLocations.getJumpInstructionLabel(second)
        Some(LabelledLocations.ifIntegerCompareNotEquals(target))
      case IfNotZero.`shape` =>
        val target = LabelledLocations.getJumpInstructionLabel(second)
        Some(LabelledLocations.ifIntegerCompareEquals(target))
      case _ => None
    }
  }

  def findNotReplacement(second: Node): Option[Node] = {
    second.shape match {
      case IfZeroDelta.`shape` =>
        val target = LabelledLocations.getJumpInstructionLabel(second)
        Some(LabelledLocations.ifNotZero(target))
      case IfNotZero.`shape` =>
        val target = LabelledLocations.getJumpInstructionLabel(second)
        Some(LabelledLocations.ifZero(target))
      case _ => None
    }
  }

  def findGreaterThanReplacement(second: Node): Option[Node] = {
    second.shape match {
      case IfZeroDelta.`shape` =>
        val target = LabelledLocations.getJumpInstructionLabel(second)
        Some(LabelledLocations.ifIntegerCompareLessEquals(target))
      case IfNotZero.`shape` =>
        val target = LabelledLocations.getJumpInstructionLabel(second)
        Some(LabelledLocations.ifIntegerCompareGreater(target))
      case _ => None
    }
  }

  def findLessThanReplacement(second: Node): Option[Node] = {
    second.shape match {
      case IfZeroDelta.`shape` =>
        val target = LabelledLocations.getJumpInstructionLabel(second)
        Some(LabelledLocations.ifIntegerCompareGreaterEquals(target))
      case IfNotZero.`shape` =>
        val target = LabelledLocations.getJumpInstructionLabel(second)
        Some(LabelledLocations.ifIntegerCompareLess(target))
      case _ => None
    }
  }

  override def description: String = "Combines instructions to reduce the instruction count. " +
    "Applies mostly to custom instructions related to comparison operations, followed by a jumps."
}
