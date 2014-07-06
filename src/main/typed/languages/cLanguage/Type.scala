package typed.languages.cLanguage

abstract class Type {
  def size : Int
}

object CDouble extends Type{
  def size: Int = 32
}
object CInt extends Type{
  def size: Int = 4
}
object CFloat extends Type{
  def size: Int = 16
}
object CChar extends Type {
  def size = 4
}
object CBool extends Type {
  def size = 4
}
object CString extends PointerType(CChar)

case class PointerType(on: Type) extends Type{
  def size: Int = CInt.size
}

class ArrayType(on: Type, count: Int) extends PointerType(on)
{
  override def size: Int = on.size * count
}

case class FunctionType(returnType: Type, parameterTypes: Seq[Type]) extends Type{
  def size: Int = 1
}

case class StructType(fields: Map[String,Type])
