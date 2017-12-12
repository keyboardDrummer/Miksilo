package application.graphing.model.simplifications

import core.deltas.Contract
import deltas.javac.JavaCompilerDeltas

object ByteCode extends DeltaGroup {

  override def dependencies: Set[Contract] = JavaCompilerDeltas.byteCodeDeltas.toSet

  override def dependants: Set[Contract] = Set.empty //Set(LabelledTargets, ExpandInstructionsC)
}
