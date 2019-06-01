package application.graphing.model.simplifications

import core.deltas.Contract
import deltas.javac.ByteCodeLanguage

object ByteCodeWithTypes extends DeltaGroup {
  override def dependants: Set[Contract] = Set(ByteCode)

  override def dependencies: Set[Contract] = ByteCodeLanguage.types.toSet
}
