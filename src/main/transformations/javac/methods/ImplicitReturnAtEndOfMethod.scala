package transformations.javac.methods

import core.transformation.sillyCodePieces.ProgramTransformation
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.javac.classes.ClassC

object ImplicitReturnAtEndOfMethod extends ProgramTransformation {
  override def dependencies: Set[Contract] = Set(ReturnVoidC, ReturnExpressionC)

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    val clazz = program
    val methods = ClassC.getMethods(clazz)
    for (method <- methods) {
      val statements = MethodC.getMethodBody(method)
      val hasNoReturn = statements.isEmpty ||
        (statements.last.clazz != ReturnExpressionC.ReturnInteger && statements.last.clazz != ReturnVoidC.ReturnVoidKey)
      if (hasNoReturn) {
        method(MethodC.MethodBodyKey) = statements ++ Seq(ReturnVoidC._return)
      }
    }
  }
}
