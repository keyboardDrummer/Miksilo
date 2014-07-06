package typed.languages.cLanguage

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
  def getType(machine: CMachine): Type = CInt
}

case class LiteralChar(value: Char) extends Literal{
  def getType(machine: CMachine): Type = CChar
}

case class LiteralString(value: String) extends Literal{
  def getType(machine: CMachine): Type = CString

  override def evaluate(machine: CMachine): Any = {
    machine.memory.putAlloc(value, getType(machine))
  }
}

case class LiteralFloat(value: Float) extends Literal{
  def getType(machine: CMachine): Type = CFloat
}

case class LiteralDouble(value: Double) extends Literal{
  def getType(machine: CMachine): Type = CDouble
}

case class LiteralArray[T <: Literal](values: Seq[T]) extends Expression {
  def getType(machine: CMachine): Type = values(0).getType(machine)

  def evaluate(machine: CMachine): Any = {
    val _type = getType(machine)
    val size = _type.size
    val location = machine.memory.heapAlloc(values.size * size)
    Stream.from(0).map(index => machine.memory.put(location + index * size, values(index), _type))
  }
}
