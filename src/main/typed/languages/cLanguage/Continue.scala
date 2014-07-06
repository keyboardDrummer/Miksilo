package typed.languages.cLanguage

object Continue extends Statement {
  def execute(machine: CMachine): StatementResult = ContinueResult
}

object LoopBreak extends Statement{
  def execute(machine: CMachine): StatementResult = BreakResult
}