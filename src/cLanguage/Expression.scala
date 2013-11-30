package cLanguage

abstract class Expression extends Statement {
  def execute(machine: CMachine): StatementResult = {
    evaluate()
    Done
  }
  def evaluate(machine: CMachine) : Any
}

case class LiteralInt(value: Int) extends Literal

case class LiteralChar(value: Char) extends Literal

case class LiteralString(value: String) extends Literal

case class LiteralFloat(value: Float) extends Literal

case class LiteralDouble(value: Double) extends Literal

case class LiteralArray(values: Seq[Literal]) extends Literal

trait Literal extends Expression

case class Assignment(target: HiddenPointer, value: Expression)

trait HiddenPointer extends Expression

case class Identifier(value: String) extends HiddenPointer

case class Dereference(target: Expression) extends HiddenPointer

case class Select(target: Expression, selector: Identifier) extends HiddenPointer

case class GetAddress(target: HiddenPointer) extends Expression

case class IndexArray(target: Expression) extends HiddenPointer

case class Variable(name: String) extends HiddenPointer