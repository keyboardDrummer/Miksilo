package cLanguage

case class Block(statements: Seq[Statement]) extends Statement {
  def execute(machine: CMachine): Unit = {
    machine.runBlock(statements)
  }
}
