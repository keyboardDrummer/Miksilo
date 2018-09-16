package deltas.javac.statements

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.{NodePath, SequenceElement}
import core.language.node._
import core.language.{Compilation, Language}
import deltas.bytecode.ByteCodeMethodInfo
import deltas.bytecode.simpleBytecode.{InferredStackFrames, LabelDelta, LabelledLocations}
import deltas.javac.expressions.ToByteCodeSkeleton
import deltas.statement.IfThenDelta
import deltas.statement.IfThenDelta.getThenStatement

object IfThenToByteCodeDelta extends ByteCodeStatementInstance {

  override val shape = IfThenDelta.Shape

  override def dependencies: Set[Contract] = super.dependencies ++ Set(IfThenDelta)

  override def toByteCode(ifThen: NodePath, compilation: Compilation): Seq[Node] = {
    val condition = IfThenDelta.getCondition(ifThen)
    val method = ifThen.findAncestorShape(ByteCodeMethodInfo.Shape)
    val endLabelName = LabelDelta.getUniqueLabel("ifEnd", method)
    val end = InferredStackFrames.label(endLabelName)
    val body = IfThenDelta.getThenStatement(ifThen)

    val jumpToEndIfFalse = LabelledLocations.ifZero(endLabelName)
    val toInstructionsExpr = ToByteCodeSkeleton.getToInstructions(compilation)
    val toInstructionsStatement = ByteCodeStatementSkeleton.getToInstructions(compilation)
    toInstructionsExpr(condition) ++
      Seq(jumpToEndIfFalse) ++
      toInstructionsStatement(body) ++
      Seq(end)
  }

  override def description: String = "Translates if-then statements to bytecode"

  override def getNextStatements(language: Language, obj: NodePath, labels: Map[Any, NodePath]): Set[NodePath] =
  {
    val thenStatement = getThenStatement(obj)
    val thenNextStatements = ByteCodeStatementSkeleton.getInstance(language, thenStatement).getNextStatements(language, thenStatement, labels)
    thenNextStatements ++ super.getNextStatements(language, obj, labels)
  }

  override def getLabels(language: Language, obj: NodePath): Map[Any, NodePath] = {
    val thenStatement = getThenStatement(obj)
    val thenNextLabel = ByteCodeStatementSkeleton.getInstance(language, thenStatement).getNextLabel(thenStatement)

    val nextMap = obj.asInstanceOf[SequenceElement].getNext.fold[Map[Any, NodePath]](Map.empty)(
      next => Map(thenNextLabel -> next)
    ) //TODO this will not work for an if-if nesting. Should generate a next label for each statement. But this also requires labels referencing other labels.
    nextMap ++ super.getLabels(language, obj)
  }

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {}
}
