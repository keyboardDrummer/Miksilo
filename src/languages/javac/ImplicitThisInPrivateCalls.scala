package languages.javac

import transformation.{TransformationState, MetaObject, ProgramTransformation}
import languages.javac.base.{JavaBaseModel, JavaBase}

object ImplicitThisInPrivateCalls extends ProgramTransformation {
  override def dependencies: Set[ProgramTransformation] = Set(JavaBase)

  val thisName: String = "this"

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    JavaBase.getStatementToLines(state).put(JavaBaseModel.CallKey,(call : MetaObject, compiler) => {
      val callCallee = JavaBaseModel.getCallCallee(call)
      if (callCallee.clazz == JavaBaseModel.VariableKey)
      {
        val memberName = JavaBaseModel.getVariableName(callCallee)
        val currentClass = compiler.classCompiler.currentClassInfo
        val methodInfo = currentClass.getMethod(memberName)
        val selectee = JavaBaseModel.variable(if (methodInfo._static) currentClass.name else thisName)
        call(JavaBaseModel.CallCallee) = JavaBaseModel.selector(selectee, memberName)
      }
      JavaBase.callToLines(call, compiler)
    })
  }
}
