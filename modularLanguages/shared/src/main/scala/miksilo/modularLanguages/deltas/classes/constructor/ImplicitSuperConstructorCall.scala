package miksilo.modularLanguages.deltas.classes.constructor

import miksilo.modularLanguages.core.deltas.{Contract, DeltaWithPhase}
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node.Node
import miksilo.modularLanguages.deltas.classes.constructor.ConstructorDelta
import miksilo.modularLanguages.deltas.javac.constructor.{ThisCallExpression}
import miksilo.modularLanguages.deltas.javac.statements.ExpressionAsStatementDelta

object ImplicitSuperConstructorCall extends DeltaWithPhase {
  override def dependencies: Set[Contract] = Set(ConstructorDelta)

  override def transformProgram(program: Node, state: Compilation): Unit = {

    for (constructor <- ConstructorDelta.getConstructors(program)) {
      val statements = constructor.body.statements
      var addSuperCall = false
      if (statements.isEmpty)
        addSuperCall = true
      else {
        val firstStatement = statements.head
        if (firstStatement.shape != SuperCallExpression.Shape && firstStatement.shape != ThisCallExpression.Shape) {
          addSuperCall = true
        }
      }

      if (addSuperCall)
        constructor.body.statements = Seq(ExpressionAsStatementDelta.create(SuperCallExpression.superCall())) ++ statements
    }
  }

  override def description: String = "At the start of a constructor body, if no call to a super constructor is present, such a call is added."
}
