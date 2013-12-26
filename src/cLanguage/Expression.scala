package cLanguage

abstract class Expression extends Statement {
  def execute(machine: CMachine): StatementResult = {
    evaluate(machine)
    Done
  }
  def evaluate(machine: CMachine) : Any
  def _type(machine: CMachine) : Type
}

case class Assignment(target: HiddenPointer, value: Expression) extends Expression{
  def evaluate(machine: CMachine): Any = {
    machine.memory.alter(target.getAddress(machine),value.evaluate(machine))
  }

  def _type(machine: CMachine): Type = value._type(machine)
}

trait HiddenPointer extends Expression
{
  def getAddress(machine: CMachine) : Int
}

case class Dereference(target: Expression) extends HiddenPointer{

  def evaluate(machine: CMachine): Any = {
    val pointer = target.evaluate(machine).asInstanceOf[Int]
    machine.memory(pointer)
  }

  def _type(machine: CMachine): Type = target._type(machine).asInstanceOf[PointerType].on

  def getAddress(machine: CMachine): Int = target.evaluate(machine).asInstanceOf[Int]
}

case class Select(target: Expression, selector: String) extends HiddenPointer{

  def evaluate(machine: CMachine): Any = ???

  def _type(machine: CMachine): Type = target._type(machine).asInstanceOf[StructType].fields(selector)

  def getAddress(machine: CMachine): Int = ???
}

case class GetAddress(target: HiddenPointer) extends Expression{
  def evaluate(machine: CMachine): Any = target.getAddress(machine)

  def _type(machine: CMachine): Type = new PointerType(target._type(machine))
}

case class IndexArray(arrayExpression: Expression, index: Expression) extends HiddenPointer{

  def evaluate(machine: CMachine): Any = machine.memory(getAddress(machine))

  def _type(machine: CMachine): Type = arrayExpression._type(machine).asInstanceOf[PointerType].on

  def getAddress(machine: CMachine): Int = new AddPointer(arrayExpression, index).evaluate(machine)
}

case class Variable(name: String) extends HiddenPointer {
  def evaluate(machine: CMachine): Any = machine.memory(machine.env(name).location)


  def _type(machine: CMachine): Type = machine.env(name)._type

  def getAddress(machine: CMachine): Int = machine.env(name).location
}

object Variable
{
  implicit def stringToVariable(name: String) : Variable = new Variable(name)
}