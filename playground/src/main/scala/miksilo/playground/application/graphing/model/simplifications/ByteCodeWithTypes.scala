package miksilo.playground.application.graphing.model.simplifications

import miksilo.modularLanguages.core.deltas.Contract
import miksilo.modularLanguages.deltas.bytecode.ByteCodeLanguage

object ByteCodeWithTypes extends DeltaGroup {
  override def dependants: Set[Contract] = Set(ByteCode)

  override def dependencies: Set[Contract] = ByteCodeLanguage.types.toSet
}
