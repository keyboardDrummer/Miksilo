package application.graphing.model.simplifications

import core.particles.Contract
import transformations.javac.JavaCompilerDeltas

object ByteCode extends TransformationGroup {

  override def dependencies: Set[Contract] = JavaCompilerDeltas.byteCodeTransformations.toSet

  override def dependants: Set[Contract] = Set.empty //Set(LabelledTargets, ExpandInstructionsC)
}
