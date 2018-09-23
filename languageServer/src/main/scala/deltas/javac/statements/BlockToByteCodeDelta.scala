package deltas.javac.statements

import core.deltas.Contract
import core.deltas.path.NodePath
import core.language.Compilation
import core.language.node.{Node, NodeShape}
import deltas.javac.expressions.{ConvertsToByteCodeDelta, ToByteCodeSkeleton}
import deltas.statement.BlockDelta
import deltas.statement.BlockDelta.BlockStatement

object BlockToByteCodeDelta extends ConvertsToByteCodeDelta  {

  override def description: String = "Transforms a block into a sequence of bytecode instructions"

  override def dependencies: Set[Contract] = Set(BlockDelta)

  override def shape: NodeShape = BlockDelta.Shape

  override def toByteCode(statement: NodePath, compilation: Compilation): Seq[Node] = {
    val toInstructions = ToByteCodeSkeleton.getToInstructions(compilation)
    val block: BlockStatement[NodePath] = statement
    block.statements.flatMap(childStatement => toInstructions(childStatement))
  }
}
