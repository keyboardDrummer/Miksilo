package deltas.javac

import core.deltas.Delta
import deltas.bytecode.additions.PoptimizeDelta
import deltas.bytecode.extraBooleanInstructions._
import deltas.bytecode.simpleBytecode.{InferredMaxStack, InferredStackFrames, InlineConstantPool, LabelledLocations}

object ExtendedByteCode {

  def allByteCodeDeltas: Seq[Delta] = Seq(OptimizeComparisonInstructionsDelta) ++
    Seq(LessThanInstructionDelta, GreaterThanInstructionDelta, NotInstructionDelta, IntegerEqualsInstructionDelta, ExpandVirtualInstructionsDelta) ++
    simpleByteCodeDeltas

  def simpleByteCodeDeltas: Seq[Delta] = Seq(PoptimizeDelta) ++
    Seq(InferredStackFrames, InferredMaxStack, LabelledLocations, InlineConstantPool) ++ ByteCodeLanguage.byteCodeDeltas
}
