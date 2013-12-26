package cLanguage


case class Add(first: Expression, second: Expression) extends Expression{
  def typedAdd(machine: CMachine) : Expression = first._type(machine) match {
    case CInt => new AddInt(first,second)
    case _:PointerType => new AddPointer(first,second)
  }
  def evaluate(machine: CMachine): Any = typedAdd(machine).evaluate(machine)

  def _type(machine: CMachine): Type = typedAdd(machine)._type(machine)
}

case class Subtract(first: Expression, second: Expression) extends Expression{
  def typedAdd(machine: CMachine) : Expression = first._type(machine) match {
    case CInt => new SubtractInt(first,second)
    case _:PointerType => new SubtractPointer(first,second)
  }
  def evaluate(machine: CMachine): Any = typedAdd(machine).evaluate(machine)

  def _type(machine: CMachine): Type = typedAdd(machine)._type(machine)
}

abstract class BinaryOperator[T](first: Expression, second: Expression) extends Expression {
  def apply(first: T, second: T) : T

  def evaluate(machine: CMachine): Any = {
    apply(first.evaluate(machine).asInstanceOf[T], second.evaluate(machine).asInstanceOf[T])
  }

  def _type(machine: CMachine): Type = {
    val firstType = first._type(machine)
    if (firstType != second._type(machine))
      throw new RuntimeException
    firstType
  }
}

class SubtractInt(first: Expression, second: Expression) extends BinaryOperator[Int](first,second) {
  def apply(first: Int, second: Int): Int = first - second
}

class AddInt(first: Expression, second: Expression) extends BinaryOperator[Int](first,second) {
  def apply(first: Int, second: Int): Int = first + second
}

case class AddPointer(pointer: Expression, offset: Expression) extends OffsetPointer(pointer,offset) {
  def applyOffset(location: Int, offset: Int): Int = location + offset
}

case class SubtractPointer(pointer: Expression, offset: Expression) extends OffsetPointer(pointer,offset) {
  def applyOffset(location: Int, offset: Int): Int = location - offset
}

abstract class OffsetPointer(pointer: Expression, offset: Expression) extends Expression {
  def applyOffset(location: Int, offset: Int) : Int

  def evaluate(machine: CMachine): Int = {
    val pointerValue = pointer.evaluate(machine).asInstanceOf[Int]
    val size = _type(machine).on.size
    val offsetValue = offset.evaluate(machine).asInstanceOf[Int]
    applyOffset(pointerValue, size * offsetValue)
  }

  def _type(machine: CMachine): PointerType = pointer._type(machine).asInstanceOf[PointerType]
}
