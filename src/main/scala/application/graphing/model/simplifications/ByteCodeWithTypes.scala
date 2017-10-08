package application.graphing.model.simplifications

import core.particles.Contract
import transformations.bytecode.types._
import transformations.javac.JavaCompilerDeltas
import transformations.javac.types.BooleanTypeC

object ByteCodeWithTypes extends TransformationGroup {
  override def dependants: Set[Contract] = Set(ByteCode)

  override def dependencies: Set[Contract] = JavaCompilerDeltas.typeTransformations.toSet
}
