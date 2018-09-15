package application.graphing.model.simplifications

import core.deltas.Contract
import deltas.javac.classes.skeleton.JavaClassSkeleton
import deltas.javac.statements._
import deltas.javac.statements.locals.LocalDeclarationDelta

object JavaSimpleStatement extends DeltaGroup {

  override def dependencies: Set[Contract] = Set(LocalDeclarationDelta, IfThenElseToByteCodeDelta, IfThenToByteCodeDelta,
    ForLoopContinueDelta, ExpressionAsStatementDelta, ForLoopDelta)

  override def dependants: Set[Contract] = Set(JavaClassSkeleton)
}
