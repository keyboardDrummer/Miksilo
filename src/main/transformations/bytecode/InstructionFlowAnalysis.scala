package transformations.bytecode

import core.transformation.MetaObject
import transformations.bytecode.instructions._
import util.DataFlowAnalysis

abstract class InstructionFlowAnalysis[State](instructions: Seq[MetaObject]) extends DataFlowAnalysis[Int, State] {

  val labels = instructions.zipWithIndex.filter(i => i._1.clazz == LabelledJumps.LabelKey)
    .map(p => (LabelledJumps.getLabelName(p._1), p._2)).toMap

  override def getOutgoingNodes(instructionIndex: Int): Set[Int] = {
    val instruction = instructions(instructionIndex)
    instruction.clazz match {
      case GotoC.GoToKey => Set(labels(LabelledJumps.getGoToTarget(instruction)))
      case IfZeroC.IfZeroKey =>
        val target = LabelledJumps.getIfIntegerCompareGreaterTarget(instruction)
        Set(instructionIndex + 1, labels(target))
      case IfIntegerCompareGreaterC.IfIntegerCompareGreaterKey =>
        val target = LabelledJumps.getIfIntegerCompareGreaterTarget(instruction)
        Set(instructionIndex + 1, labels(target))
      case VoidReturnC.VoidReturn => Set()
      case IntegerReturnC.IntegerReturn => Set()
      case _ => Set(instructionIndex + 1)
    }
  }
}
