package transformations.bytecode

import core.transformation.{Contract, MetaObject, ProgramTransformation, TransformationState}
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.instructions.GotoC.GoToKey
import transformations.bytecode.instructions.IfIntegerCompareGreaterC.IfIntegerCompareGreaterKey
import transformations.bytecode.instructions.IfZeroC.IfZeroKey
import transformations.bytecode.instructions.{GotoC, IfIntegerCompareGreaterC, IfZeroC}
import transformations.javac.base.ConstantPool

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object LabelledJumps extends ProgramTransformation {
  def ifZero(target: String) = instruction(IfZeroC.IfZeroKey, Seq(target))

  def goTo(target: String) = instruction(GotoC.GoToKey, Seq(target))

  def ifIntegerCompareGreater(target: String) = instruction(IfIntegerCompareGreaterC.IfIntegerCompareGreaterKey, Seq(target))

  def label(name: String, stackFrame: MetaObject) = new MetaObject(LabelKey) {
    data.put(LabelNameKey, name)
    data.put(LabelStackFrame, stackFrame)
  }

  override def inject(state: TransformationState): Unit = {
    ByteCodeSkeleton.getInstructionSignatureRegistry(state).put(LabelKey, (pool, label) => (Seq.empty, Seq.empty))
    ByteCodeSkeleton.getInstructionStackSizeModificationRegistry(state).put(LabelKey, (pool, label) => 0)
    ByteCodeSkeleton.getInstructionSizeRegistry(state).put(LabelKey, 0)
  }

  def transform(program: MetaObject, state: TransformationState): Unit = {

    def instructionSize(instruction: MetaObject) = ByteCodeSkeleton.getInstructionSizeRegistry(state)(instruction.clazz)

    def getNewInstructions(instructions: Seq[MetaObject], targetLocations: Map[String, Int]): ArrayBuffer[MetaObject] = {
      var newInstructions = mutable.ArrayBuffer[MetaObject]()
      newInstructions.sizeHint(instructions.length)

      var location = 0
      for (instruction <- instructions) {

        instruction.clazz match {
          case GoToKey => setInstructionArguments(instruction, Seq(
            targetLocations(getGoToTarget(instruction)) - location))
          case IfIntegerCompareGreaterKey => setInstructionArguments(instruction, Seq(
            targetLocations(getIfIntegerCompareGreaterTarget(instruction)) - location))
          case IfZeroKey => setInstructionArguments(instruction, Seq(
            targetLocations(getIfZeroTarget(instruction)) - location))
          case _ =>
        }
        if (instruction.clazz != LabelKey)
          newInstructions += instruction

        location += instructionSize(instruction)
      }

      newInstructions
    }

    val clazz = program
    val constantPool = new ConstantPool(ByteCodeSkeleton.getConstantPool(clazz))
    for (codeAnnotation <- ByteCodeSkeleton.getMethods(clazz)
      .flatMap(methodInfo => ByteCodeSkeleton.getMethodAttributes(methodInfo))
      .flatMap(annotation => if (annotation.clazz == ByteCodeSkeleton.CodeKey) Some(annotation) else None)) {
      val instructions = ByteCodeSkeleton.getCodeInstructions(codeAnnotation)
      val targetLocations: Map[String, Int] = determineTargetLocations(instructions)
      codeAnnotation(ByteCodeSkeleton.CodeAttributesKey) = ByteCodeSkeleton.getCodeAttributes(codeAnnotation) ++ getStackMapTable(constantPool, targetLocations, instructions)

      val newInstructions: Seq[MetaObject] = getNewInstructions(instructions, targetLocations)
      codeAnnotation(ByteCodeSkeleton.CodeInstructionsKey) = newInstructions
    }

    def determineTargetLocations(instructions: Seq[MetaObject]): Map[String, Int] = {
      val targetLocations = mutable.Map[String, Int]()
      var location = 0
      for (instruction <- instructions) {
        instruction.clazz match {
          case LabelKey => targetLocations(getLabelName(instruction)) = location
          case _ =>
        }

        location += instructionSize(instruction)
      }
      targetLocations.toMap
    }
  }

  def getStackMapTable(constantPool: ConstantPool, labelLocations: Map[String, Int], instructions: Seq[MetaObject]): Seq[MetaObject] = {
    val frameLocations = instructions.filter(i => i.clazz == LabelKey).map(i => (labelLocations(getLabelName(i)), getLabelStackFrame(i)))
    var adjustedZero = 0
    var stackFrames = ArrayBuffer[MetaObject]()
    stackFrames.sizeHint(frameLocations.size)
    for (framePair <- frameLocations) {
      val location = framePair._1
      val frame = framePair._2
      val offset = location - adjustedZero

      frame(ByteCodeSkeleton.OffsetDelta) = offset
      stackFrames += frame
      adjustedZero = location + 1
    }
    if (stackFrames.nonEmpty) {
      val nameIndex = constantPool.store(ByteCodeSkeleton.StackMapTableId)
      Seq(ByteCodeSkeleton.stackMapTable(nameIndex, stackFrames))
    }
    else
      Seq[MetaObject]()
  }

  def getLabelStackFrame(label: MetaObject) = label(LabelStackFrame).asInstanceOf[MetaObject]

  def getLabelName(label: MetaObject) = label(LabelNameKey).asInstanceOf[String]

  def getGoToTarget(goTo: MetaObject) = getInstructionArguments(goTo)(0).asInstanceOf[String]

  def getIfIntegerCompareGreaterTarget(compare: MetaObject) =
    getInstructionArguments(compare)(0).asInstanceOf[String]

  def getIfZeroTarget(ifZero: MetaObject) =
    getInstructionArguments(ifZero)(0).asInstanceOf[String]

  override def dependencies: Set[Contract] = Set(ByteCode)

  object LabelKey

  object LabelNameKey

  object LabelStackFrame

}
