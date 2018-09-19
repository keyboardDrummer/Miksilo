package deltas.javac.statements

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node._
import core.language.{Compilation, Language}
import deltas.bytecode.ByteCodeMethodInfo
import deltas.bytecode.simpleBytecode.{InferredStackFrames, LabelDelta, LabelledLocations}
import deltas.javac.expressions.ToByteCodeSkeleton
import deltas.statement.IfThenDelta
import deltas.statement.IfThenDelta.IfThen

object IfThenToByteCodeDelta extends ByteCodeStatementInstance {

  override val shape = IfThenDelta.Shape

  override def dependencies: Set[Contract] = super.dependencies ++ Set(IfThenDelta)

  override def toByteCode(statement: NodePath, compilation: Compilation): Seq[Node] = {
    val ifThen: IfThen[NodePath] = statement
    val method = ifThen.findAncestorShape(ByteCodeMethodInfo.Shape)
    val endLabelName = LabelDelta.getUniqueLabel("ifEnd", method)
    val end = InferredStackFrames.label(endLabelName)
    val jumpToEndIfFalse = LabelledLocations.ifZero(endLabelName)
    val toInstructionsExpr = ToByteCodeSkeleton.getToInstructions(compilation)
    val toInstructionsStatement = ByteCodeStatementSkeleton.getToInstructions(compilation)
    toInstructionsExpr(ifThen.condition) ++
      Seq(jumpToEndIfFalse) ++
      toInstructionsStatement(ifThen.thenStatement) ++
      Seq(end)
  }

  override def description: String = "Translates if-then statements to bytecode"

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {}
}
