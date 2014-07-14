package transformations.javac

import core.transformation.{MetaObject, ProgramTransformation, TransformationState}
import transformations.javac.base.model.{JavaClassModel, JavaMethodModel}
import transformations.javac.methods.ReturnC

object ImplicitReturnAtEndOfMethod extends ProgramTransformation {
  override def dependencies: Set[ProgramTransformation] = Set(ReturnC)

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    val clazz = program
    val methods = JavaClassModel.getMethods(clazz)
    for (method <- methods) {
      val instructions = JavaMethodModel.getMethodBody(method)
      if (instructions.isEmpty || instructions.last.clazz != ReturnC.Return) {
        method(JavaMethodModel.MethodBodyKey) = instructions ++ Seq(ReturnC._return())
      }
    }
  }
}
