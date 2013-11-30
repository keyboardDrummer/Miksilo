package cLanguage

case class Call(callee: Expression, arguments: Seq[Expression] = Seq.empty) extends Expression {
  def evaluate(machine: CMachine): Any =
  {
    val function = machine.evaluate(callee).asInstanceOf[Function]
    val functionStack = new StackFrame()
    machine.stack.push(functionStack)
    val result = executeFunctionBody(machine, function)
    machine.stack.pop()
    result
  }

  def executeFunctionBody(machine: CMachine, function: Function) : StatementResult = {
    for (statement <- machine.getStatements(function.body)) {
      val result = statement.execute(machine)
      result match {
        case ReturnResult(value) => return ReturnResult(value)
      }
    }
    Done
  }
}
