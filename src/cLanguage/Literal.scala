package cLanguage

abstract class Literal extends Expression
{
  def value: Any
  def evaluate(machine: CMachine): Any = value
}

object LiteralInt
{
  implicit def intToLiteral(value: Int) : LiteralInt = new LiteralInt(value)
}

case class LiteralInt(value: Int) extends Literal {
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
