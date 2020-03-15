package miksilo.playground.application.graphing.model.simplifications

import miksilo.modularLanguages.core.deltas.Contract
import miksilo.modularLanguages.deltas.bytecode.ByteCodeLanguage

object ByteCode extends DeltaGroup {

  override def dependencies: Set[Contract] = ByteCodeLanguage.byteCodeDeltas.toSet

  override def dependants: Set[Contract] = Set.empty //Set(LabelledTargets, ExpandInstructionsC)
}
