package deltas.javac.methods

import core.deltas.node.Node
import core.deltas.{Compilation, Contract, DeltaWithPhase, Language}

object ImplicitReturnAtEndOfMethod extends DeltaWithPhase {
  override def dependencies: Set[Contract] = Set(ReturnVoidC, ReturnExpressionC)

  override def transform(program: Node, state: Compilation): Unit = {
    val clazz = program
    val methods = MethodDelta.getMethods(clazz)
    for (method <- methods) {
      val statements = MethodDelta.getMethodBody(method)
      val hasNoReturn = statements.isEmpty ||
        (statements.last.clazz != ReturnExpressionC.ReturnInteger && statements.last.clazz != ReturnVoidC.ReturnVoidKey)
      if (hasNoReturn) {
        method(MethodDelta.Body) = statements ++ Seq(ReturnVoidC._return)
      }
    }
  }

  override def description: String = "Adds an implicit return at the end of a method if none is present."
}
