package languages.javac

import transformation.{TransformationState, MetaObject, ProgramTransformation}
import languages.javac.base.{JavaBaseModel, JavaBase}

object ImplicitThisInPrivateCalls extends ProgramTransformation {
  override def dependencies: Set[ProgramTransformation] = Set(JavaBase)

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    JavaBase.getStatementToLines(state).put(JavaBaseModel.CallKey,(call : MetaObject, compiler) => {
      val callCallee = JavaBaseModel.getCallCallee(call)
      if (callCallee.clazz == JavaBaseModel.VariableKey)
      {
        val memberName = JavaBaseModel.getVariableName(callCallee)
        call(JavaBaseModel.CallCallee) = JavaBaseModel.selector(JavaBaseModel.variable("this"), memberName)
      }
      JavaBase.callToLines(call, compiler)
    })
  }
}
