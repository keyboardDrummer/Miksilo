package application.graphing.model.simplifications

import core.particles.Contract
import transformations.bytecode.types._
import transformations.javac.JavaCompiler
import transformations.javac.types.BooleanTypeC

object ByteCodeWithTypes extends TransformationGroup {
  override def dependants: Set[Contract] = Set(ByteCode)

  override def dependencies: Set[Contract] = JavaCompiler.typeTransformations.toSet
}
