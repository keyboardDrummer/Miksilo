package transformations.javac

import core.transformation.sillyCodePieces.ParticleWithPhase
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.javac.classes.{SelectorC, ClassC}
import transformations.javac.expressions.ExpressionC
import transformations.javac.methods.{CallC, VariableC}

object ImplicitThisInPrivateCalls extends ParticleWithPhase {
  val thisName: String = "this"

  override def dependencies: Set[Contract] = Set(CallC, VariableC, ClassC)

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    val original = ExpressionC.getExpressionToLines(state)(CallC.CallKey)
    ExpressionC.getExpressionToLines(state).put(CallC.CallKey, (call: MetaObject) => {
      val callCallee = CallC.getCallCallee(call)
      val compiler = ClassC.getClassCompiler(state)
      if (callCallee.clazz == VariableC.VariableKey) {
        val memberName = VariableC.getVariableName(callCallee)
        val currentClass = compiler.currentClassInfo
        val methodInfo = currentClass.getMethod(memberName)
        val selectee = VariableC.variable(if (methodInfo._static) currentClass.name else thisName)
        call(CallC.CallCallee) = SelectorC.selector(selectee, memberName)
      }
      original(call)
    })
  }
}
