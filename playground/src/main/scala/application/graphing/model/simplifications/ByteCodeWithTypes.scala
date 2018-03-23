package application.graphing.model.simplifications

import core.deltas.Contract
import deltas.bytecode.types._
import deltas.javac.JavaLanguage
import deltas.javac.types.BooleanTypeDelta

object ByteCodeWithTypes extends DeltaGroup {
  override def dependants: Set[Contract] = Set(ByteCode)

  override def dependencies: Set[Contract] = JavaLanguage.typeTransformations.toSet
}
