package typed.languages.cLanguage

case class VariableDeclaration(name: String, _type: Type, mbValue: Option[Expression] = None) extends Statement {
  def this(name: String, _type: Type, value: Expression) { this(name, _type, Some(value)) }

  def execute(machine: CMachine): StatementResult = {
    val location = machine.memory.stackAlloc(_type.size)
    machine.env.put(name, new EnvEntry(location, _type))
    mbValue.foreach(v => machine.memory.alter(machine.env(name).location, v.evaluate(machine)))
    Done
  }
}
