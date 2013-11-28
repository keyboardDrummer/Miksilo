package cLanguage

class Statement {

}

case class Switch(value: Expression, cases: Seq[SwitchCase])

case class SwitchCase(expression: Expression, body: Block)