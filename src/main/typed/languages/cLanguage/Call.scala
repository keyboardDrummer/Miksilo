package typed.languages.cLanguage

case class Call(callee: Expression, arguments: Seq[Expression] = Seq.empty) extends Expression {
  override def execute(machine: CMachine): StatementResult =
  {
    val function = callee.evaluate(machine).asInstanceOf[Function]
    machine.env.push()
    val result = executeFunctionBody(machine, function)
    machine.env.pop()
    result
  }

  def executeFunctionBody(machine: CMachine, function: Function) : StatementResult = {
    for (statement <- function.body.statements) {
      val result = statement.execute(machine)
      result match {
        case ReturnResult(value) => return ReturnResult(value)
        case _ =>
      }
    }
    Done
  }

  def evaluate(machine: CMachine): Any ={
    val result = execute(machine)
    result match {
      case ReturnResult(value) => value
      case _ => throw new RuntimeException("function call expression returned nothing")
    }
  }

  def getType(machine: CMachine): Type = callee.getType(machine).asInstanceOf[FunctionType].returnType
}
