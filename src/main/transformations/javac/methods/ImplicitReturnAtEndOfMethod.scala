package transformations.javac.methods

import core.transformation.sillyCodePieces.ProgramTransformation
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.javac.classes.ClassC

object ImplicitReturnAtEndOfMethod extends ProgramTransformation {
  override def dependencies: Set[Contract] = Set(ReturnVoidC)

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    val clazz = program
    val methods = ClassC.getMethods(clazz)
    for (method <- methods) {
      val instructions = MethodC.getMethodBody(method)
      if (instructions.isEmpty || instructions.last.clazz != ReturnExpressionC.ReturnInteger) {
        method(MethodC.MethodBodyKey) = instructions ++ Seq(ReturnVoidC._return())
      }
    }
  }
}
