package application.graphing.model.simplifications

import core.deltas.Contract
import deltas.javac.ByteCodeLanguage

object ByteCode extends DeltaGroup {

  override def dependencies: Set[Contract] = ByteCodeLanguage.byteCodeDeltas.toSet

  override def dependants: Set[Contract] = Set.empty //Set(LabelledTargets, ExpandInstructionsC)
}
