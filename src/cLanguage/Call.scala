package cLanguage

case class Call(callee: Expression, arguments: Seq[Expression] = Seq.empty) extends Expression {
  def execute(machine: CMachine): Unit =
  {
    val function = machine.evaluate(callee)
    val functionStack = new StackFrame()
    machine.stack.push(functionStack)
    functionStack.scheduledStatements ++=
  }
}
