package cLanguage

abstract class Expression extends Statement {
  def execute(machine: CMachine): StatementResult = {
    evaluate(machine)
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

abstract class Pointer()
{
  def dereference(machine: CMachine) : Any
  def assignValue(machine: CMachine, value: Any) : Unit
}

case class Add(first: Expression, second: Expression) extends Expression{
  def evaluate(machine: CMachine): Any ={
    val firstType = machine.getType(first)
    firstType match {
      case PointerType => {

      }
    }
  }
}

trait Literal extends Expression

case class Assignment(target: HiddenPointer, value: Expression)

trait HiddenPointer extends Expression
{
  def address : Expression
}

case class Identifier(value: String)
case class Dereference(target: Expression) extends HiddenPointer{
  def address: Expression = target

  def evaluate(machine: CMachine): Any = {
    val pointer = evaluate(target)
  }
}

case class Select(target: Expression, selector: Identifier) extends HiddenPointer

case class GetAddress(target: HiddenPointer) extends Expression{
  def evaluate(machine: CMachine): Any = target.address
}

class ArrayPointer extends Pointer
{
  def dereference(machine: CMachine): Any = ???
}

case class IndexArray(arrayExpression: Expression, index: Expression) extends HiddenPointer{
  def address: Expression = {
    new Add(arrayExpression, index)
  }

  def evaluate(machine: CMachine): Any = new Dereference(address).evaluate(machine)
}

case class Variable(name: String) extends HiddenPointer with Pointer {
  def evaluate(machine: CMachine): Any =  {}

  def address: Expression = this

  def dereference(machine: CMachine): Any = machine.

  def assignValue(machine: CMachine, value: Any): Unit = ???
}