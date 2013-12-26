package cLanguage

class While(condition: Expression, body: Block) extends Statement {
  def execute(machine: CMachine): StatementResult = {
    while(condition.evaluate(machine).asInstanceOf[Boolean])
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
        case _:ReturnResult => return result
        case BreakResult => return result
        case Done =>
      }
    }
    Done
  }
}
