package transformations.javac

import core.transformation.{MetaObject, ProgramTransformation, TransformationState}
import transformations.javac.base.JavaMethodC
import transformations.javac.expressions.ExpressionC
import transformations.javac.methods.{CallC, SelectorC, VariableC}

object ImplicitThisInPrivateCalls extends ProgramTransformation {
  override def dependencies: Set[ProgramTransformation] = Set(CallC, VariableC)

  val thisName: String = "this"

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    ExpressionC.getExpressionToLines(state).put(CallC.CallKey, (call: MetaObject) => {
      val callCallee = CallC.getCallCallee(call)
      val compiler = JavaMethodC.getMethodCompiler(state)
      if (callCallee.clazz == VariableC.VariableKey) {
        val memberName = VariableC.getVariableName(callCallee)
        val currentClass = compiler.classCompiler.currentClassInfo
        val methodInfo = currentClass.getMethod(memberName)
        val selectee = VariableC.variable(if (methodInfo._static) currentClass.name else thisName)
        call(CallC.CallCallee) = SelectorC.selector(selectee, memberName)
      }
      CallC.callToLines(call, compiler)
    })
  }
}
