package miksilo.modularLanguages.deltas.javac

import miksilo.modularLanguages.core.deltas.Delta
import miksilo.modularLanguages.deltas.bytecode.ByteCodeLanguage
import miksilo.modularLanguages.deltas.bytecode.additions.PoptimizeDelta
import miksilo.modularLanguages.deltas.bytecode.extraBooleanInstructions._
import miksilo.modularLanguages.deltas.bytecode.extraConstants.TypeConstant
import miksilo.modularLanguages.deltas.bytecode.simpleBytecode.{InferredMaxStack, InferredStackFrames, InlineConstantPool, LabelledLocations}

object ExtendedByteCode {

  def allByteCodeDeltas: Seq[Delta] = Seq(OptimizeComparisonInstructionsDelta) ++
    Seq(LessThanInstructionDelta, GreaterThanInstructionDelta, NotInstructionDelta, IntegerEqualsInstructionDelta, ExpandVirtualInstructionsDelta) ++
    Seq(TypeConstant) ++ simpleByteCodeDeltas

  def simpleByteCodeDeltas: Seq[Delta] = Seq(PoptimizeDelta) ++
    Seq(InferredStackFrames, InferredMaxStack, LabelledLocations, InlineConstantPool) ++ ByteCodeLanguage.byteCodeDeltas
}
