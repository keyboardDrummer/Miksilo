package miksilo.modularLanguages.deltas.javac.expressions.prefix

import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node.Node
import miksilo.modularLanguages.deltas.bytecode.extraBooleanInstructions.NotInstructionDelta
import miksilo.modularLanguages.deltas.expression.logical.LogicalNotDelta
import miksilo.modularLanguages.deltas.javac.expressions.{ConvertsToByteCodeDelta, ToByteCodeSkeleton}

object LogicalNotToByteCode extends ConvertsToByteCodeDelta {

  override def toByteCode(expression: NodePath, compilation: Compilation): Seq[Node] = {
    ToByteCodeSkeleton.getToInstructions(compilation)(expression) ++ Seq(NotInstructionDelta.not)
  }

  override def description = "Converts logical not to bytecode"

  override def shape = LogicalNotDelta.Shape

  override def dependencies = Set(LogicalNotDelta, NotInstructionDelta)
}
