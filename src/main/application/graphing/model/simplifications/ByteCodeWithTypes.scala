package application.graphing.model.simplifications

import core.transformation.Contract
import transformations.types._

object ByteCodeWithTypes extends TransformationGroup {
  override def dependants: Set[Contract] = Set.empty

  override def dependencies: Set[Contract] = Set(ObjectTypeC, ArrayTypeC, BooleanTypeC, DoubleTypeC, LongTypeC, VoidTypeC, IntTypeC, IntTypeC)
}
