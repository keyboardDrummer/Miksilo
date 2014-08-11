package transformations.javac.methods

import core.transformation.sillyCodePieces.ProgramTransformation
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.javac.base.{MethodAndClassC, MethodPart}

object ImplicitReturnAtEndOfMethod extends ProgramTransformation {
  override def dependencies: Set[Contract] = Set(ReturnVoidC)

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    val clazz = program
    val methods = MethodAndClassC.getMethods(clazz)
    for (method <- methods) {
      val instructions = MethodPart.getMethodBody(method)
      if (instructions.isEmpty || instructions.last.clazz != ReturnExpressionC.ReturnInteger) {
        method(MethodPart.MethodBodyKey) = instructions ++ Seq(ReturnVoidC._return())
      }
    }
  }
}
