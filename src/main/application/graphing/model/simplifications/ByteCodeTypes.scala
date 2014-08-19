package application.graphing.model.simplifications

import core.transformation.Contract
import transformations.bytecode.ByteCodeSkeleton
import transformations.types._

object ByteCodeTypes extends TransformationGroup {
  override def dependants: Set[Contract] = Set(ByteCodeSkeleton)

  override def dependencies: Set[Contract] = Set(ObjectTypeC, ArrayTypeC, BooleanTypeC, DoubleTypeC, LongTypeC, VoidTypeC, IntTypeC, IntTypeC)
}
