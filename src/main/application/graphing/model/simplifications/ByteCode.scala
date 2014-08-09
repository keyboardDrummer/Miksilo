package application.graphing.model.simplifications

import core.transformation.Contract
import transformations.bytecode.LabelledTargets
import transformations.javac.JavaCompiler

object ByteCode extends Simplification {
  override def dependants: Set[Contract] = Set(LabelledTargets)

  override def dependencies: Set[Contract] = JavaCompiler.byteCodeInstructions.toSet
}
