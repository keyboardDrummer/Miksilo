package typed.languages.cLanguage

trait Statement {
  def execute(machine: CMachine) : StatementResult
}

case class Block(statements: Seq[Statement])

case class Switch(value: Expression, cases: Seq[SwitchCase])

case class SwitchCase(expression: Expression, body: Block)