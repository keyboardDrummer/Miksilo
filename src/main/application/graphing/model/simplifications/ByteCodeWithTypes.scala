package application.graphing.model.simplifications

import core.particles.Contract
import transformations.bytecode.types._
import transformations.javac.types.BooleanTypeC

object ByteCodeWithTypes extends TransformationGroup {
  override def dependants: Set[Contract] = Set.empty

  override def dependencies: Set[Contract] = Set(ObjectTypeC, ArrayTypeC, BooleanTypeC, DoubleTypeC, LongTypeC, VoidTypeC, IntTypeC, IntTypeC)
}
