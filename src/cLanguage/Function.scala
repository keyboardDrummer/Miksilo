package cLanguage

case class FunctionParameter(name: String, _type: Type)

case class Function(returnType: Type, parameters: Seq[FunctionParameter], body: Block) {

}

case class Return(value: Expression) extends Statement