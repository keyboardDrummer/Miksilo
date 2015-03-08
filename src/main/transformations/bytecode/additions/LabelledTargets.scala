package transformations.bytecode.additions

import core.transformation.{ParticleWithPhase, Contract, MetaObject, CompilationState}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.attributes.{CodeAttribute, StackMapTableAttribute}
import transformations.bytecode.coreInstructions.integers.integerCompare.IfNotZero.IfNotZeroKey
import transformations.bytecode.coreInstructions.integers.integerCompare._
import transformations.bytecode.coreInstructions.{InstructionSignature, GotoC, InstructionC}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.javac.classes.ConstantPool

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object LabelledTargets extends ParticleWithPhase {
  def ifZero(target: String) = instruction(IfZeroC.IfZeroKey, Seq(target))
  def ifNotZero(target: String) = instruction(IfNotZeroKey, Seq(target))

  def goTo(target: String) = instruction(GotoC.GoToKey, Seq(target))

  def ifIntegerCompareGreaterEquals(target: String) = instruction(IfIntegerCompareGreaterOrEqualC.IfIntegerCompareGreaterKey, Seq(target))
  def ifIntegerCompareLess(target: String) = instruction(IfIntegerCompareLessC.key, Seq(target))
  def ifIntegerCompareEquals(target: String) = instruction(IfIntegerCompareEqualC.key, Seq(target))
  def ifIntegerCompareNotEquals(target: String) = instruction(IfIntegerCompareNotEqualC.key, Seq(target))

  def label(name: String, stackFrame: MetaObject) = new MetaObject(LabelKey) {
    data.put(LabelNameKey, name)
    data.put(LabelStackFrame, stackFrame)
  }

  override def inject(state: CompilationState): Unit = {
    super.inject(state)
    LabelC.inject(state)
  }

  def transform(program: MetaObject, state: CompilationState): Unit = {

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
    val constantPool = ByteCodeSkeleton.getConstantPool(clazz)
    val codeAnnotations: Seq[MetaObject] = CodeAttribute.getCodeAnnotations(clazz)

    for (codeAnnotation <- codeAnnotations) {
      processCodeAnnotation(codeAnnotation)
    }

    def processCodeAnnotation(codeAnnotation: MetaObject): Option[Any] = {
      val instructions = CodeAttribute.getCodeInstructions(codeAnnotation)
      val targetLocations: Map[String, Int] = determineTargetLocations(instructions)
      codeAnnotation(CodeAttribute.CodeAttributesKey) = CodeAttribute.getCodeAttributes(codeAnnotation) ++ getStackMapTable(constantPool, targetLocations, instructions)

      val newInstructions: Seq[MetaObject] = getNewInstructions(instructions, targetLocations)
      codeAnnotation(CodeAttribute.CodeInstructionsKey) = newInstructions
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

      frame(StackMapTableAttribute.OffsetDelta) = offset
      stackFrames += frame
      adjustedZero = location + 1
    }
    if (stackFrames.nonEmpty) {
      val nameIndex = constantPool.store(StackMapTableAttribute.stackMapTableId)
      Seq(StackMapTableAttribute.stackMapTable(nameIndex, stackFrames))
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

    override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject, typeState: ProgramTypeState, state: CompilationState):
    InstructionSignature = new InstructionSignature(Seq.empty, Seq.empty)

    override def getInstructionSize(instruction: MetaObject): Int = 0

    override def description: String = "Used to mark a specific point in an instruction list."
  }

  object LabelKey

  object LabelNameKey

  object LabelStackFrame

  override def description: String = "Replaces the jump instructions from bytecode. " +
    "The new instructions are similar to the old ones except that they use labels as target instead of instruction indices."

}
