package languages.bytecode

import util.DataFlowAnalysis
import transformation.MetaObject

abstract class InstructionFlowAnalysis[State](instructions: Seq[MetaObject]) extends DataFlowAnalysis[Int, State] {

  val labels = instructions.zipWithIndex.filter(i => i._1.clazz == ByteCodeGoTo.LabelKey)
    .map(p => (ByteCodeGoTo.getLabelName(p._1), p._2)).toMap

  override def getOutgoingNodes(instructionIndex: Int): Set[Int] = {
    val instruction = instructions(instructionIndex)
    instruction.clazz match {
      case ByteCode.GoToKey => Set(labels(ByteCodeGoTo.getGoToTarget(instruction)))
      case ByteCode.IfZeroKey =>
        val target = ByteCodeGoTo.getIfIntegerCompareGreaterTarget(instruction)
        Set(instructionIndex + 1, labels(target))
      case ByteCode.IfIntegerCompareGreater =>
        val target = ByteCodeGoTo.getIfIntegerCompareGreaterTarget(instruction)
        Set(instructionIndex + 1, labels(target))
      case ByteCode.VoidReturn => Set()
      case ByteCode.IntegerReturn => Set()
      case _ => Set(instructionIndex + 1)
    }
  }
}
