package cLanguage

class Type {

}

object CDouble extends Type
object CInt extends Type
object CFloat extends Type
case class Pointer(on: Type)
case class Array(on: Type, size: Int)
