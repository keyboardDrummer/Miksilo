package transformations.javac

import core.particles.{ParticleWithPhase, Contract, MetaObject, CompilationState}
import transformations.javac.classes.{SelectorC, JavaClassSkeleton}
import transformations.javac.expressions.ExpressionSkeleton
import transformations.javac.methods.{CallC, VariableC}

object ImplicitThisInPrivateCalls extends ParticleWithPhase {
  val thisName: String = "this"

  override def dependencies: Set[Contract] = Set(CallC, VariableC, JavaClassSkeleton)

  override def transform(program: MetaObject, state: CompilationState): Unit = {
    val original = ExpressionSkeleton.getExpressionToLines(state)(CallC.CallKey)
    ExpressionSkeleton.getExpressionToLines(state).put(CallC.CallKey, (call: MetaObject) => {
      val callCallee = CallC.getCallCallee(call)
      val compiler = JavaClassSkeleton.getClassCompiler(state)
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

  override def description: String = "Implicitly prefixes references to private methods with the 'this' qualified if it is missing."
}
