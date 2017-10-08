package transformations.javac.constructor

import core.particles.node.Node
import core.particles.{Compilation, Contract, DeltaWithPhase, Language}
import transformations.javac.methods.MethodDelta
import transformations.javac.statements.ExpressionAsStatementC

object ImplicitSuperConstructorCall extends DeltaWithPhase {
  override def dependencies: Set[Contract] = Set(ConstructorC)

  override def transform(clazz: Node, state: Compilation): Unit = {

    for (constructor <- ConstructorC.getConstructors(clazz)) {
      val statements = MethodDelta.getMethodBody(constructor)
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
        constructor(MethodDelta.MethodBodyKey) = Seq(ExpressionAsStatementC.create(SuperCallExpression.superCall())) ++ statements
    }
  }

  override def description: String = "At the start of a constructor body, if no call to a super constructor is present, such a call is added."
}
