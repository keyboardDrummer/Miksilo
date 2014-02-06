package languages

import transformation.MetaObject
import ssm._

object SSM {
  def toTyped(untypedSSM: MetaObject) : SSMProgram = {
    val instructions = untypedSSM(lines).asInstanceOf[Seq[MetaObject]]
    new SSMProgram(instructions.map(instructionToTyped))
  }

  private def instructionToTyped(untypedInstruction: MetaObject) : Instruction = {
    val clazz = untypedInstruction(SSM.clazz).asInstanceOf[Class[_]]
    val arguments = untypedInstruction(SSM.arguments).asInstanceOf[Seq[AnyRef]].toArray
    val constructor = clazz.getConstructors()(0)
    constructor.newInstance(arguments:_*).asInstanceOf[Instruction]
  }

  private def createInstruction[T <: Instruction](clazz : Class[T], arguments: Any*) = {
    val result = new MetaObject(instruction)
    result(SSM.clazz) = clazz
    result(SSM.arguments) = arguments
    result
  }

  val arguments = "arguments"
  val clazz = "class"
  val instruction = "instruction"
  val lines = "lines"

  def createLabel(target: String) = createInstruction(classOf[Label],target)
  def jumpOnFalse(target: String) = createInstruction(classOf[JumpOnFalse],target)
  def jumpOnTrue(target: String) = createInstruction(classOf[JumpOnTrue],target)
  def jumpAlways(target: String) = createInstruction(classOf[JumpAlways], target)
  def loadRegister(index: Int) = createInstruction(classOf[LoadRegister],index)
  def loadFreeRegister(index: Int) = loadRegister(index + 4)
  def loadConstant(value: Int) = createInstruction(classOf[LoadConstant], value)
  def loadTrue() = loadConstant(1)
  def loadFalse() = loadConstant(0)
  def addition = createInstruction(classOf[Addition])
  def storeRegister(index: Int) = createInstruction(classOf[StoreRegister],index)
  def storeFreeRegister(index: Int) = storeRegister(index + 4)
  def notEquals = createInstruction(classOf[NotEquals])
  def lessThen = createInstruction(classOf[LessThen])
}
