package cLanguage

abstract class Expression extends Statement {
  def execute(machine: CMachine): StatementResult = {
    evaluate(machine)
    Done
  }
  def evaluate(machine: CMachine) : Any
  def _type(machine: CMachine) : Type
}

case class LiteralInt(value: Int) extends Literal{
  def _type(machine: CMachine): Type = CInt
}

case class LiteralChar(value: Char) extends Literal{
  def _type(machine: CMachine): Type = ???
}

case class LiteralString(value: String) extends Literal{
  def _type(machine: CMachine): Type = ???
}

case class LiteralFloat(value: Float) extends Literal{
  def _type(machine: CMachine): Type = CFloat
}

case class LiteralDouble(value: Double) extends Literal{
  def _type(machine: CMachine): Type = CDouble
}

case class LiteralArray[T <: Literal](values: Seq[T]) extends Expression {
  def _type(machine: CMachine): Type = ???

  def evaluate(machine: CMachine): Any = ???
}

case class Add(first: Expression, second: Expression) extends Expression{
  def typedAdd(machine: CMachine) : Expression = first._type(machine) match {
    case CInt => new AddInt(first,second)
    case _:PointerType => new AddPointer(first,second)
  }
  def evaluate(machine: CMachine): Any = typedAdd(machine).evaluate(machine)

  def _type(machine: CMachine): Type = typedAdd(machine)._type(machine)
}

case class AddInt(first: Expression, second: Expression) extends Expression {
  def evaluate(machine: CMachine): Any = {
    first.evaluate(machine).asInstanceOf[Int] + second.evaluate(machine).asInstanceOf[Int]
  }

  def _type(machine: CMachine): Type = {
    val firstType = first._type(machine)
    if (firstType != second._type(machine))
      throw new RuntimeException
    firstType
  }
}

case class AddPointer(pointer: Expression, offset: Expression) extends Expression {
  def evaluate(machine: CMachine): Any = {
    val pointerValue = pointer.evaluate(machine).asInstanceOf[Int]
    val size = _type(machine).on.size
    val offsetValue = offset.evaluate(machine).asInstanceOf[Int]
    pointerValue + size * offsetValue
  }

  def _type(machine: CMachine): PointerType = pointer._type(machine).asInstanceOf[PointerType]
}

trait Literal extends Expression
{
  def evaluate(machine: CMachine): Any = this
}

case class Assignment(target: HiddenPointer, value: Expression)

trait HiddenPointer extends Expression
{
  def address : Expression
}

case class Dereference(target: Expression) extends HiddenPointer{
  def address: Expression = target

  def evaluate(machine: CMachine): Any = {
    val pointer = target.evaluate(machine).asInstanceOf[Int]
    machine.memory(pointer)
  }

  def _type(machine: CMachine): Type = target._type(machine).asInstanceOf[PointerType].on
}

case class Select(target: Expression, selector: String) extends HiddenPointer{
  def address: Expression = ???

  def evaluate(machine: CMachine): Any = ???

  def _type(machine: CMachine): Type = target._type(machine).asInstanceOf[StructType].fields(selector)
}

case class GetAddress(target: HiddenPointer) extends Expression{
  def evaluate(machine: CMachine): Any = target.address

  def _type(machine: CMachine): Type = new PointerType(target._type(machine))
}

case class IndexArray(arrayExpression: Expression, index: Expression) extends HiddenPointer{
  def address: Expression = new AddPointer(arrayExpression, index)

  def evaluate(machine: CMachine): Any = new Dereference(address).evaluate(machine)

  def _type(machine: CMachine): Type = arrayExpression._type(machine).asInstanceOf[PointerType].on
}

case class VariablePointer(name: String) extends Expression
{
  def evaluate(machine: CMachine): Int = machine.env(name).location

  def _type(machine: CMachine): Type = new PointerType(machine.env(name)._type)
}

case class Variable(name: String) extends HiddenPointer {
  def evaluate(machine: CMachine): Any = machine.memory(machine.env(name).location)

  def address: Expression = new VariablePointer(name)

  def _type(machine: CMachine): Type = machine.env(name)._type
}