package miksilo.modularLanguages.deltas.javac.statements

import miksilo.modularLanguages.core.deltas._
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node._
import miksilo.modularLanguages.deltas.bytecode.ByteCodeMethodInfo
import miksilo.modularLanguages.deltas.bytecode.simpleBytecode.{InferredStackFrames, LabelDelta, LabelledLocations}
import miksilo.modularLanguages.deltas.javac.expressions.{ConvertsToByteCodeDelta, ToByteCodeSkeleton}
import miksilo.modularLanguages.deltas.statement.IfThenDelta
import miksilo.modularLanguages.deltas.statement.IfThenDelta.IfThen

object IfThenToByteCodeDelta extends ConvertsToByteCodeDelta {

  override def description: String = "Compiles if-then statements to bytecode"

  override val shape = IfThenDelta.Shape

  override def dependencies: Set[Contract] = Set(IfThenDelta)

  override def toByteCode(statement: NodePath, compilation: Compilation): Seq[Node] = {
    val ifThen: IfThen[NodePath] = statement
    val methodInfo = ifThen.findAncestorShape(ByteCodeMethodInfo.Shape)
    val endLabelName = LabelDelta.getUniqueLabel("ifEnd", methodInfo)
    val end = InferredStackFrames.label(endLabelName)
    val jumpToEndIfFalse = LabelledLocations.ifZero(endLabelName)
    val toInstructionsExpr = ToByteCodeSkeleton.getToInstructions(compilation)
    val toInstructionsStatement = ToByteCodeSkeleton.getToInstructions(compilation)
    toInstructionsExpr(ifThen.condition) ++
      Seq(jumpToEndIfFalse) ++
      toInstructionsStatement(ifThen.thenStatement) ++
      Seq(end)
  }
}
