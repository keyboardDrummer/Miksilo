package cLanguage

import util.StackedMap
import scala.collection.immutable.Queue
import scala.collection.mutable


class StackFrame {
  val env = Map[String,Any]
}

abstract class StatementResult
object ContinueResult extends StatementResult
object BreakResult extends StatementResult
case class ReturnResult(value: Any) extends StatementResult
object Done extends StatementResult

class CMachine {
  val env = new StackedMap[String, Any]()
  env.push()

  val stack = mutable.Stack[StackFrame]()

  def getStatements(statement: Statement) : Seq[Statement] {

  }
  def run(program: CProgram) {
    for(function <- program.functions)
      env.put(function.name, function)

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

