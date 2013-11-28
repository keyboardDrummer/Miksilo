package cLanguage

class Expression extends Statement {

}

case class LiteralInt(value: Int)
case class LiteralChar(value: Char)
case class LiteralString(value: String)
case class LiteralFloat(value: Float)
case class LiteralDouble(value: Double)

case class Assignment(target: Assignable, value: Expression)

trait Assignable
case class Identifier(value: String) extends Expression with Assignable
case class Dereference(target: Expression, selector: Identifier) extends Expression with Assignable