package application.graphing.model.simplifications

import core.particles.Contract
import transformations.javac.JavaCompiler

object ByteCode extends TransformationGroup {

  override def dependencies: Set[Contract] = JavaCompiler.byteCodeTransformations.toSet

  override def dependants: Set[Contract] = Set.empty //Set(LabelledTargets, ExpandInstructionsC)
}
