package languages.bytecode
import ByteCode._
import transformation.{TransformationState, ProgramTransformation, MetaObject}
import scala.collection.mutable

object ByteCodeGoTo extends ProgramTransformation {
  def goTo(target: String) = instruction(ByteCode.GoToKey, Seq(target))
  def ifIntegerCompareGreater(target: String) = instruction(ByteCode.IfIntegerCompareGreater, Seq(target))
  def getGoToTarget(goTo: MetaObject) = getInstructionArguments(goTo)(0).asInstanceOf[String]
  def getIfIntegerCompareGreaterTarget(compare: MetaObject) =
    getInstructionArguments(compare)(0).asInstanceOf[String]

  object LabelKey
  object LabelNameKey
  def label(name: String) = new MetaObject(LabelKey) {
    data.put(LabelNameKey,name)
  }
  def getLabelName(label: MetaObject) = label(LabelNameKey).asInstanceOf[String]

  def instructionSize(instruction: MetaObject) = instruction.clazz match {
    case LabelKey => 0
    case IfIntegerCompareGreater => 3
    case GoToKey => 3
    case IntegerIncrementKey => 3

    case ByteCode.IntegerConstantKey => 1
    case ByteCode.AddIntegersKey => 1
    case _ => 1
  }

  def transform(program: MetaObject, state: TransformationState): Unit = {

    for(codeAnnotation <- ByteCode.getMethods(program)
      .flatMap(methodInfo => ByteCode.getMethodAnnotations(methodInfo))
      .flatMap(annotation => if (annotation.clazz == ByteCode.CodeKey) Some(annotation) else None))
    {
      val instructions = ByteCode.getCodeInstructions(codeAnnotation)
      val targetLocations = mutable.Map[String,Int]()
      var location = 0
      for(instruction <- instructions)
      {
        instruction.clazz match {
          case LabelKey => targetLocations(getLabelName(instruction)) = location
          case _ =>
        }

        location += instructionSize(instruction)
      }

      var newInstructions = mutable.ArrayBuffer[MetaObject]()
      newInstructions.sizeHint(instructions.length)
      for(instruction <- instructions)
      {
        instruction.clazz match {
          case GoToKey => setInstructionArguments(instruction, Seq(targetLocations(getGoToTarget(instruction))))
          case IfIntegerCompareGreater => setInstructionArguments(instruction, Seq(
            targetLocations(getIfIntegerCompareGreaterTarget(instruction))))
          case _ =>
        }
        if (instruction.clazz != LabelKey)
          newInstructions += instruction
      }
      codeAnnotation(ByteCode.CodeInstructionsKey) = newInstructions
    }
  }

  def dependencies: Set[ProgramTransformation] = Set(ByteCode)
}
