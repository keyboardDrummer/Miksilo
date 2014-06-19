package languages.javac

import transformation.{TransformationState, MetaObject, ProgramTransformation}
import languages.bytecode.ByteCode
import languages.javac.base.JavaBase
import javaInterpreter.JavaClass
import languages.javac.base.model.{JavaMethodModel, JavaClassModel}

object ImplicitReturnAtEndOfMethod extends ProgramTransformation {
  override def dependencies: Set[ProgramTransformation] = Set(JavaBase)

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    val clazz = program
    val methods = JavaClassModel.getMethods(clazz)
    for(method <- methods)
    {
      val instructions = JavaMethodModel.getMethodBody(method)
      if (instructions.isEmpty || instructions.last.clazz != JavaMethodModel.Return)
      {
        method(JavaMethodModel.MethodBodyKey) = instructions ++ Seq(JavaMethodModel._return())
      }
    }
  }
}
