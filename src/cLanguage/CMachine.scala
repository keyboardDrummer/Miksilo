package cLanguage

import util.StackedMap
import scala.collection.immutable.Queue
import scala.collection.mutable


class StackFrame {
  val scheduledStatements = mutable.Queue.empty[Statement]
}

class CMachine {
  val env = new StackedMap[String, Any]()
  env.push()

  val stack = mutable.Stack[StackFrame]()
  var returnValue

  def run(program: CProgram) {
    for(function <- program.functions)
      env.put(function.name, function)

    val initialStatement = new Call(new Variable("main"))
    initialStatement.execute(this)
  }

  def runBlock(statements: Seq[Statement]){
    statements.foreach(statement => scheduledStatements.enqueue(statement))
  }

  def evaluate(expression: Expression) : Any = {

  }

  def returnFunction(expression: Expression) {
    scheduledStatements.clear()
  }

  def callFunction(function: Function)
  {

  }
}

class Continuation
{

}

