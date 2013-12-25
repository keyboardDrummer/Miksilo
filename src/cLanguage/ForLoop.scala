package cLanguage

case class ForLoop(initializer: Expression, condition: Expression, increment: Expression, body: Block) extends Statement {
  def execute(machine: CMachine): StatementResult = {
    machine.evaluate(initializer)
    while(machine.evaluate(condition).asInstanceOf[Boolean])
    {
      val result = runIteration(machine)
      result match {
        case BreakResult => return Done
        case ReturnResult(value) => return result
      }
    }
    Done
  }

  def runIteration(machine: CMachine) : StatementResult = {
    for (statement <- body.statements) {
      val result = statement.execute(machine)
      result match {
        case ContinueResult => return Done
        case Done =>
        case _ => result
      }
    }
    machine.evaluate(increment)
    Done
  }
}
