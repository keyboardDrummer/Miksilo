package cLanguage

import util.StackedMap
import scala.collection.immutable.Queue
import scala.collection.mutable


class StackFrame {
  val env = Map[String,Any]()
}

abstract class StatementResult
object ContinueResult extends StatementResult
object BreakResult extends StatementResult
case class ReturnResult(value: Any) extends StatementResult
object Done extends StatementResult

case class EnvEntry(location: Int, _type: Type)
class CMachine {
  val memory = new Memory()
  val env = new StackedMap[String, EnvEntry]()
  env.push()

  val stack = mutable.Stack[StackFrame]()

  def getType(expression: Expression) : Type = {
    expression._type(this)
  }

  def getStatements(statement: Statement) : Seq[Statement] = ???

  def run(program: CProgram) {
    for(function <- program.functions)
    {
      val _type = function._type(this)
      val location = memory.putAlloc(function,_type)
      env.put(function.name, new EnvEntry(location,_type))
    }

    val initialStatement = new Call(new Variable("main"))
    initialStatement.execute(this)
  }


  def evaluate(expression: Expression) : Any = {

  }

  def callFunction(function: Function)
  {

  }
}

class Continuation
{

}

