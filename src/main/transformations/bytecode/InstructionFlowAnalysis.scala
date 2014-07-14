package transformations.bytecode

import core.transformation.MetaObject
import util.DataFlowAnalysis

abstract class InstructionFlowAnalysis[State](instructions: Seq[MetaObject]) extends DataFlowAnalysis[Int, State] {

  val labels = instructions.zipWithIndex.filter(i => i._1.clazz == LabelledJumps.LabelKey)
    .map(p => (LabelledJumps.getLabelName(p._1), p._2)).toMap

  override def getOutgoingNodes(instructionIndex: Int): Set[Int] = {
    val instruction = instructions(instructionIndex)
    instruction.clazz match {
      case ByteCode.GoToKey => Set(labels(LabelledJumps.getGoToTarget(instruction)))
      case ByteCode.IfZeroKey =>
        val target = LabelledJumps.getIfIntegerCompareGreaterTarget(instruction)
        Set(instructionIndex + 1, labels(target))
      case ByteCode.IfIntegerCompareGreater =>
        val target = LabelledJumps.getIfIntegerCompareGreaterTarget(instruction)
        Set(instructionIndex + 1, labels(target))
      case ByteCode.VoidReturn => Set()
      case ByteCode.IntegerReturn => Set()
      case _ => Set(instructionIndex + 1)
    }
  }
}
