package miksilo.modularLanguages.deltas.bytecode.simpleBytecode

import miksilo.languageServer.util.DataFlowAnalysis
import miksilo.modularLanguages.core.node.Node
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.InstructionInstance.Instruction
import miksilo.modularLanguages.deltas.bytecode.simpleBytecode.LabelDelta.Label

abstract class InstructionFlowAnalysis[State](instructions: Seq[Instruction[Node]])
  extends DataFlowAnalysis[Int, State] {
  
  val labelIndices = instructions.zipWithIndex.
    filter(indexedInstruction => indexedInstruction._1.shape == LabelDelta.Shape).
    map(indexedInstruction => (new Label(indexedInstruction._1.node).name, indexedInstruction._2)).toMap

  override def getOutgoingNodes(instructionIndex: Int): Set[Int] = {
    val instruction = instructions(instructionIndex)

    val jumpBehavior = instruction.jumpBehavior
    var result = Set.empty[Int]
    if (jumpBehavior.movesToNext)
      result += instructionIndex + 1

    if (jumpBehavior.hasJumpInFirstArgument)
      result += labelIndices(LabelledLocations.getJumpInstructionLabel(instruction))

    result
  }
}
