package application.graphing.model.simplifications

import core.deltas.Contract
import deltas.bytecode.types._
import deltas.javac.JavaCompilerDeltas
import deltas.javac.types.BooleanTypeC

object ByteCodeWithTypes extends TransformationGroup {
  override def dependants: Set[Contract] = Set(ByteCode)

  override def dependencies: Set[Contract] = JavaCompilerDeltas.typeTransformations.toSet
}
