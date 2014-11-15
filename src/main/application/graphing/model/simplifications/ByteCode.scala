package application.graphing.model.simplifications

import core.transformation.Contract
import transformations.bytecode.additions.LabelledTargets
import transformations.bytecode.extraBooleanInstructions.ExpandInstructionsC
import transformations.javac.JavaCompiler

object ByteCode extends TransformationGroup {
  override def dependants: Set[Contract] = Set(LabelledTargets, ExpandInstructionsC)

  override def dependencies: Set[Contract] = JavaCompiler.byteCodeInstructions.toSet
}
