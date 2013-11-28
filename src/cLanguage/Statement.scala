package cLanguage

trait Statement {
  def execute(machine: CMachine)
}

case class Switch(value: Expression, cases: Seq[SwitchCase])

case class SwitchCase(expression: Expression, body: Block)