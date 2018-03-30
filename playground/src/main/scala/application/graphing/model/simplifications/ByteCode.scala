package application.graphing.model.simplifications

import core.deltas.Contract
import deltas.javac.JavaLanguage

object ByteCode extends DeltaGroup {

  override def dependencies: Set[Contract] = JavaLanguage.byteCodeDeltas.toSet

  override def dependants: Set[Contract] = Set.empty //Set(LabelledTargets, ExpandInstructionsC)
}
