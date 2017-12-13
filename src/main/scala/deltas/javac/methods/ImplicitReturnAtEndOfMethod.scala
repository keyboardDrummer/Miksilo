package deltas.javac.methods

import core.deltas.node.Node
import core.deltas.{Compilation, Contract, DeltaWithPhase}
import deltas.javac.classes.skeleton.JavaClassSkeleton.JavaClass

object ImplicitReturnAtEndOfMethod extends DeltaWithPhase {
  override def dependencies: Set[Contract] = Set(ReturnVoidDelta, ReturnExpressionDelta)

  override def transformProgram(program: Node, state: Compilation): Unit = {
    val javaClass: JavaClass[Node] = program
    val methods = MethodDelta.getMethods(javaClass)
    for (method <- methods) {
      val statements = method.body
      val hasNoReturn = statements.isEmpty ||
        (statements.last.shape != ReturnExpressionDelta.ReturnInteger && statements.last.shape != ReturnVoidDelta.ReturnVoidKey)
      if (hasNoReturn) {
        method(MethodDelta.Body) = statements ++ Seq(ReturnVoidDelta._return)
      }
    }
  }

  override def description: String = "Adds an implicit return at the end of a method if none is present."
}
