package cLanguage

case class FunctionParameter(name: String, _type: Type)

case class Function(name: String, returnType: Type, parameters: Seq[FunctionParameter], body: Block) extends Literal {
  def _type(machine: CMachine): Type = new FunctionType(returnType, parameters.map(p => p._type))
}

case class Return(value: Expression) extends Statement{
  def execute(machine: CMachine): StatementResult = new ReturnResult(machine.evaluate(value))
}