package deltas.javac.statements

import core.deltas._
import core.deltas.path.NodePath
import core.language.Compilation
import core.language.node._
import deltas.bytecode.ByteCodeMethodInfo
import deltas.bytecode.simpleBytecode.{InferredStackFrames, LabelDelta, LabelledLocations}
import deltas.javac.expressions.{ConvertsToByteCodeDelta, ToByteCodeSkeleton}
import deltas.statement.IfThenDelta
import deltas.statement.IfThenDelta.IfThen

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
