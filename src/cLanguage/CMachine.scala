package cLanguage

import util.StackedMap
import scala.collection.immutable.Queue
import scala.collection.mutable
import java.io.StringWriter


class StackFrame {
  val env = Map[String,Any]()
}


abstract class StatementResult
object ContinueResult extends StatementResult
object BreakResult extends StatementResult
case class ReturnResult(value: Any) extends StatementResult
object Done extends StatementResult

class FunctionStack(machine: CMachine) extends  mutable.Map[String,EnvEntry] {
  val env = new StackedMap[String, EnvEntry]()
  def push() {
    env.push()
  }

  def pop()
  {
    for(entry <- env.stack(0).values)
      machine.memory.remove(entry.location)
    env.pop()
  }

  def get(key: String): Option[EnvEntry] = env.get(key)

  def iterator: Iterator[(String, EnvEntry)] = env.iterator

  def +=(kv: (String, EnvEntry)): this.type = { env.+=(kv); this }

  def -=(key: String): this.type = { env.-=(key); this }
}

case class EnvEntry(location: Int, _type: Type)
class CMachine {
  val memory = new Memory()
  val env = new FunctionStack(this)
  val standardOut = new StringWriter()

  def getType(expression: Expression) : Type = {
    expression.getType(this)
  }

  def run(program: CProgram) {
    env.push()

    for(function <- program.functions)
    {
      val _type = function.getType(this)
      val location = memory.putAlloc(function,_type)
      env.put(function.name, new EnvEntry(location,_type))
    }

    for(global <- program.globalVariables)
    {
      global.execute(this)
    }

    val initialStatement = new Call(new Variable("main"))
    initialStatement.execute(this)
  }
}

