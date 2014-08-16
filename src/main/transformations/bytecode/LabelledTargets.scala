package transformations.bytecode

import core.transformation.sillyCodePieces.ProgramTransformation
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.coreInstructions.integers.integerCompare.IfNotZero.IfNotZeroKey
import transformations.bytecode.coreInstructions.integers.integerCompare.{IfIntegerCompareGreaterOrEqualC, IfIntegerCompareLessC, IfZeroC}
import transformations.bytecode.coreInstructions.{GotoC, InstructionC}
import transformations.javac.classes.ConstantPool

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object LabelledTargets extends ProgramTransformation {
  def ifZero(target: String) = instruction(IfZeroC.IfZeroKey, Seq(target))
  def ifNotZero(target: String) = instruction(IfNotZeroKey, Seq(target))

  def goTo(target: String) = instruction(GotoC.GoToKey, Seq(target))

  def ifIntegerCompareGreaterEquals(target: String) = instruction(IfIntegerCompareGreaterOrEqualC.IfIntegerCompareGreaterKey, Seq(target))
  def ifIntegerCompareLess(target: String) = instruction(IfIntegerCompareLessC.key, Seq(target))

  def label(name: String, stackFrame: MetaObject) = new MetaObject(LabelKey) {
    data.put(LabelNameKey, name)
    data.put(LabelStackFrame, stackFrame)
  }

  override def inject(state: TransformationState): Unit = {
    LabelC.inject(state)
  }

  def transform(program: MetaObject, state: TransformationState): Unit = {

    val jumpRegistry = ByteCodeSkeleton.getState(state).jumpBehaviorRegistry
    def instructionSize(instruction: MetaObject) = ByteCodeSkeleton.getInstructionSizeRegistry(state)(instruction.clazz)(instruction)

    def getNewInstructions(instructions: Seq[MetaObject], targetLocations: Map[String, Int]): ArrayBuffer[MetaObject] = {
      var newInstructions = mutable.ArrayBuffer[MetaObject]()
      newInstructions.sizeHint(instructions.length)

      var location = 0
      for (instruction <- instructions) {

        if (jumpRegistry(instruction.clazz).hasJumpInFirstArgument) {
          setInstructionArguments(instruction, Seq(targetLocations(getJumpInstructionLabel(instruction)) - location))
        }

        if (instruction.clazz != LabelKey)
          newInstructions += instruction

        location += instructionSize(instruction)
      }

      newInstructions
    }

    val clazz = program
    val constantPool = new ConstantPool(ByteCodeSkeleton.getConstantPool(clazz))
    val codeAnnotations: Seq[MetaObject] = getCodeAnnotations(clazz)

    for (codeAnnotation <- codeAnnotations) {
      processCodeAnnotation(codeAnnotation)
    }

    def processCodeAnnotation(codeAnnotation: MetaObject): Option[Any] = {
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

  def getJumpInstructionLabel(instruction: MetaObject): String = {
    getInstructionArguments(instruction)(0).asInstanceOf[String]
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

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton, IfIntegerCompareGreaterOrEqualC, GotoC, IfZeroC)

  object LabelC extends InstructionC {
    override val key: AnyRef = LabelKey

    override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = throw new UnsupportedOperationException()

    override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject, state: TransformationState): (Seq[MetaObject], Seq[MetaObject]) = (Seq.empty, Seq.empty)

    override def getInstructionSize(instruction: MetaObject): Int = 0
  }

  object LabelKey

  object LabelNameKey

  object LabelStackFrame

}
