package playground.application.graphing.model.simplifications

import core.deltas.Contract
import miksilo.modularLanguages.deltas.javac.classes.skeleton.JavaClassDelta
import miksilo.modularLanguages.deltas.javac.statements._
import miksilo.modularLanguages.deltas.statement.{ForLoopDelta, LocalDeclarationDelta}

object JavaSimpleStatement extends DeltaGroup {

  override def dependencies: Set[Contract] = Set(LocalDeclarationDelta, IfThenElseToIfThenAndGotoDelta, IfThenToByteCodeDelta,
    ForLoopContinueDelta, ExpressionAsStatementDelta, ForLoopDelta)

  override def dependants: Set[Contract] = Set(JavaClassDelta)
}
