package application.graphing.model.simplifications

import core.transformation.Contract
import transformations.javac.base.JavaMethodC
import transformations.javac.statements.{BlockC, DeclarationC, StatementC, WhileC}

object JavaSimpleStatement extends Simplification {

  override def dependencies: Set[Contract] = Set(WhileC, StatementC, BlockC, DeclarationC)

  override def dependants: Set[Contract] = Set(JavaMethodC, JavaStatement)
}
