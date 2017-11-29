package deltas.bytecode.simpleBytecode

import core.deltas.node.{Node, NodeClass}
import deltas.bytecode.attributes.CodeAttribute
import deltas.bytecode.attributes.CodeAttribute.JumpBehavior
import deltas.bytecode.simpleBytecode.LabelDelta.Label
import util.DataFlowAnalysis

abstract class InstructionFlowAnalysis[State](instructions: Seq[Node])
  extends DataFlowAnalysis[Int, State] {

  def getJumpBehavior(instructionClazz: NodeClass): JumpBehavior
  
  val labelIndices = instructions.zipWithIndex.
    filter(indexedInstruction => indexedInstruction._1.clazz == LabelDelta.LabelKey).
    map(indexedInstruction => (new Label(indexedInstruction._1).name, indexedInstruction._2)).toMap

  override def getOutgoingNodes(instructionIndex: Int): Set[Int] = {
    val instruction = instructions(instructionIndex)

    val jumpBehavior = getJumpBehavior(instruction.clazz)
    var result = Set.empty[Int]
    if (jumpBehavior.movesToNext)
      result += instructionIndex + 1

    if (jumpBehavior.hasJumpInFirstArgument)
      result += labelIndices(CodeAttribute.getInstructionArguments(instruction).head.asInstanceOf[String])

    result
  }
}
