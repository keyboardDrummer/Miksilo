package application.graphing.model.simplifications

import core.transformation.Contract
import transformations.javac.methods.ReturnC
import transformations.javac.statements.{DeclarationC, WhileC}

object JavaStatement extends TransformationGroup {

  override def dependencies: Set[Contract] = Set(JavaExpression, WhileC, DeclarationC, ReturnC)

  override def dependants: Set[Contract] = Set()
}
