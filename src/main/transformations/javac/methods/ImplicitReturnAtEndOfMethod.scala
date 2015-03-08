package transformations.javac.methods

import core.transformation.{ParticleWithPhase, Contract, MetaObject, CompilationState}

object ImplicitReturnAtEndOfMethod extends ParticleWithPhase {
  override def dependencies: Set[Contract] = Set(ReturnVoidC, ReturnExpressionC)

  override def transform(program: MetaObject, state: CompilationState): Unit = {
    val clazz = program
    val methods = MethodC.getMethods(clazz)
    for (method <- methods) {
      val statements = MethodC.getMethodBody(method)
      val hasNoReturn = statements.isEmpty ||
        (statements.last.clazz != ReturnExpressionC.ReturnInteger && statements.last.clazz != ReturnVoidC.ReturnVoidKey)
      if (hasNoReturn) {
        method(MethodC.MethodBodyKey) = statements ++ Seq(ReturnVoidC._return)
      }
    }
  }

  override def description: String = "Adds an implicit return at the end of a method if none is present."
}
