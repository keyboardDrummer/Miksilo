package cLanguage

class Type {

}

object CDouble extends Type
object CInt extends Type
object CFloat extends Type
case class PointerType(on: Type) extends Type
case class ArrayType(on: Type, size: Int) extends Type
