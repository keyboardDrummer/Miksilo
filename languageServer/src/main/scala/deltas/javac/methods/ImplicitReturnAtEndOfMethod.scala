package deltas.javac.methods

import core.deltas.{Contract, DeltaWithPhase}
import core.language.Compilation
import core.language.node.Node
import deltas.javac.classes.skeleton.JavaClassDelta.JavaClass

object ImplicitReturnAtEndOfMethod extends DeltaWithPhase {
  override def dependencies: Set[Contract] = Set(ReturnVoidDelta, ReturnExpressionDelta)

  override def transformProgram(program: Node, state: Compilation): Unit = {
    val javaClass: JavaClass[Node] = program
    val methods = MethodDelta.getMethods(javaClass)
    for (method <- methods) {
      val statements = method.body.statements
      val hasNoReturn = statements.isEmpty ||
        (statements.last.shape != ReturnExpressionDelta.Shape && statements.last.shape != ReturnVoidDelta.Shape)
      if (hasNoReturn) {
        method.body.statements = statements ++ Seq(ReturnVoidDelta._return)
      }
    }
  }

  override def description: String = "Adds an implicit return at the end of a method if none is present."
}
