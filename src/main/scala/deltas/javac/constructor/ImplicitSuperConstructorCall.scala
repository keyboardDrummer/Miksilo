package deltas.javac.constructor

import core.deltas.node.Node
import core.deltas.{Compilation, Contract, DeltaWithPhase}
import deltas.javac.methods.MethodDelta
import deltas.javac.statements.ExpressionAsStatementC

object ImplicitSuperConstructorCall extends DeltaWithPhase {
  override def dependencies: Set[Contract] = Set(ConstructorDelta)

  override def transformProgram(clazz: Node, state: Compilation): Unit = {

    for (constructor <- ConstructorDelta.getConstructors(clazz)) {
      val statements = constructor.body
      var addSuperCall = false
      if (statements.isEmpty)
        addSuperCall = true
      else {
        val firstStatement = statements.head
        if (firstStatement.clazz != SuperCallExpression.SuperCall && firstStatement.clazz != ThisCallExpression.ThisCall) {
          addSuperCall = true
        }
      }

      if (addSuperCall)
        constructor(MethodDelta.Body) = Seq(ExpressionAsStatementC.create(SuperCallExpression.superCall())) ++ statements
    }
  }

  override def description: String = "At the start of a constructor body, if no call to a super constructor is present, such a call is added."
}
