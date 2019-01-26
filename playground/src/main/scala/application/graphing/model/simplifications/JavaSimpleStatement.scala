package application.graphing.model.simplifications

import core.deltas.Contract
import deltas.javac.classes.skeleton.JavaClassDelta
import deltas.javac.statements._
import deltas.statement.{ForLoopDelta, LocalDeclarationDelta}

object JavaSimpleStatement extends DeltaGroup {

  override def dependencies: Set[Contract] = Set(LocalDeclarationDelta, IfThenElseToIfThenAndGotoDelta, IfThenToByteCodeDelta,
    ForLoopContinueDelta, ExpressionAsStatementDelta, ForLoopDelta)

  override def dependants: Set[Contract] = Set(JavaClassDelta)
}
