package languages.bytecode

import ByteCode._
import transformation.{TransformationState, ProgramTransformation, MetaObject}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import languages.javac.base.ConstantPool

object ByteCodeGoTo extends ProgramTransformation {
  def ifZero(target: String) = instruction(ByteCode.IfZeroKey, Seq(target))

  def goTo(target: String) = instruction(ByteCode.GoToKey, Seq(target))

  def ifIntegerCompareGreater(target: String) = instruction(ByteCode.IfIntegerCompareGreater, Seq(target))

  def getGoToTarget(goTo: MetaObject) = getInstructionArguments(goTo)(0).asInstanceOf[String]

  def getIfIntegerCompareGreaterTarget(compare: MetaObject) =
    getInstructionArguments(compare)(0).asInstanceOf[String]

  def getIfZeroTarget(ifZero: MetaObject) =
    getInstructionArguments(ifZero)(0).asInstanceOf[String]

  object LabelKey

  object LabelNameKey

  object LabelStackFrame

  def label(name: String, stackFrame: MetaObject) = new MetaObject(LabelKey) {
    data.put(LabelNameKey, name)
    data.put(LabelStackFrame, stackFrame)
  }

  def getLabelName(label: MetaObject) = label(LabelNameKey).asInstanceOf[String]

  def getLabelStackFrame(label: MetaObject) = label(LabelStackFrame).asInstanceOf[MetaObject]

  def instructionSize(instruction: MetaObject) = instruction.clazz match {
    case LabelKey => 0
    case ByteCode.VoidReturn => 1
    case ByteCode.AddressLoad => 1
    case ByteCode.IntegerReturn => 1
    case ByteCode.IntegerConstantKey => 1
    case ByteCode.SubtractInteger => 1
    case ByteCode.AddIntegersKey => 1
    case ByteCode.IntegerLoad => 1
    case ByteCode.IntegerStore => 2
    case IntegerIncrementKey => 3
    case ByteCode.InvokeStaticKey => 3
    case ByteCode.InvokeVirtual => 3
    case ByteCode.InvokeSpecial => 3
    case IfIntegerCompareGreater => 3
    case ByteCode.GetStatic => 3
    case IfZeroKey => 3
    case GoToKey => 3
  }

  def getStackMapTable(constantPool: ConstantPool, labelLocations: Map[String, Int], instructions: Seq[MetaObject])
    : Seq[MetaObject] = {
    val frameLocations = instructions.filter(i => i.clazz == LabelKey).map(i => (labelLocations(getLabelName(i)), getLabelStackFrame(i)))
    var adjustedZero = 0
    var stackFrames = ArrayBuffer[MetaObject]()
    stackFrames.sizeHint(frameLocations.size)
    for (framePair <- frameLocations) {
      val location = framePair._1
      val frame = framePair._2
      val offset = location - adjustedZero

      frame(ByteCode.OffsetDelta) = offset
      stackFrames += frame
      adjustedZero = location + 1
    }
    if (stackFrames.nonEmpty) {
      val nameIndex = constantPool.store(ByteCode.StackMapTableId)
      Seq(ByteCode.stackMapTable(nameIndex, stackFrames))
    }
    else
      Seq[MetaObject]()
  }

  def transform(program: MetaObject, state: TransformationState): Unit = {
    val clazz = program
    val constantPool = new ConstantPool(ByteCode.getConstantPool(clazz))
    for (codeAnnotation <- ByteCode.getMethods(clazz)
      .flatMap(methodInfo => ByteCode.getMethodAttributes(methodInfo))
      .flatMap(annotation => if (annotation.clazz == ByteCode.CodeKey) Some(annotation) else None)) {
      val instructions = ByteCode.getCodeInstructions(codeAnnotation)
      val targetLocations: Map[String, Int] = determineTargetLocations(instructions)
      codeAnnotation(ByteCode.CodeAttributesKey) = ByteCode.getCodeAttributes(codeAnnotation) ++ getStackMapTable(constantPool, targetLocations, instructions)

      val newInstructions: Seq[MetaObject] = getNewInstructions(instructions, targetLocations)
      codeAnnotation(ByteCode.CodeInstructionsKey) = newInstructions
    }
  }


  def getNewInstructions(instructions: Seq[MetaObject], targetLocations: Map[String, Int]): ArrayBuffer[MetaObject] = {
    var newInstructions = mutable.ArrayBuffer[MetaObject]()
    newInstructions.sizeHint(instructions.length)

    var location = 0
    for (instruction <- instructions) {

      instruction.clazz match {
        case GoToKey => setInstructionArguments(instruction, Seq(
          targetLocations(getGoToTarget(instruction)) - location))
        case IfIntegerCompareGreater => setInstructionArguments(instruction, Seq(
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

  def dependencies: Set[ProgramTransformation] = Set(ByteCode)
}
