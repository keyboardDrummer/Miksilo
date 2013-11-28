package cLanguage

case class FunctionParameter(name: String, _type: Type)

case class Function(name: String, returnType: Type, parameters: Seq[FunctionParameter], body: Block) {

}

case class Return(value: Expression) extends Statement{
  def execute(machine: CMachine): Unit = {
    machine.returnValue = machine.evaluate(value)
    machine.stack(0).scheduledStatements.clear()
  }
}