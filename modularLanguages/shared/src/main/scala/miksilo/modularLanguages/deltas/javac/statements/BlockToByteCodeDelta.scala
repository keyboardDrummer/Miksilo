package miksilo.modularLanguages.deltas.javac.statements

import miksilo.modularLanguages.core.deltas.Contract
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node.{Node, NodeShape}
import miksilo.modularLanguages.deltas.javac.expressions.{ConvertsToByteCodeDelta, ToByteCodeSkeleton}
import miksilo.modularLanguages.deltas.statement.BlockDelta
import miksilo.modularLanguages.deltas.statement.BlockDelta.BlockStatement

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
