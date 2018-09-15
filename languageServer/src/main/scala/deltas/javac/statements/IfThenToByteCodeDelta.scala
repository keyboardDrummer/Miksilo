package deltas.javac.statements

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.{NodePath, SequenceElement}
import core.language.{Compilation, Language}
import core.language.node._
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import deltas.bytecode.ByteCodeMethodInfo
import deltas.bytecode.simpleBytecode.{InferredStackFrames, LabelDelta, LabelledLocations}
import deltas.javac.expressions.{ExpressionSkeleton, ToByteCodeSkeleton}
import deltas.javac.types.BooleanTypeDelta
import deltas.statement.IfThenDelta

object IfThenToByteCodeDelta extends StatementInstance {

  override val shape = IfThenDelta.Shape

  override def dependencies: Set[Contract] = super.dependencies ++ Set(IfThenDelta)

  override def toByteCode(ifThen: NodePath, compilation: Compilation): Seq[Node] = {
    val condition = IfThenDelta.getCondition(ifThen)
    val method = ifThen.findAncestorShape(ByteCodeMethodInfo.Shape)
    val endLabelName = LabelDelta.getUniqueLabel("ifEnd", method)
    val end = InferredStackFrames.label(endLabelName)
    val body = IfThenDelta.getThenStatements(ifThen)

    val jumpToEndIfFalse = LabelledLocations.ifZero(endLabelName)
    val toInstructionsExpr = ToByteCodeSkeleton.getToInstructions(compilation)
    val toInstructionsStatement = ByteCodeStatementSkeleton.getToInstructions(compilation)
    toInstructionsExpr(condition) ++
      Seq(jumpToEndIfFalse) ++
      body.flatMap(toInstructionsStatement) ++
      Seq(end)
  }

  override def description: String = "Translates if-then statements to bytecode"

  override def getNextStatements(obj: NodePath, labels: Map[Any, NodePath]): Set[NodePath] =
  {
    Set(IfThenDelta.getThenStatements(obj).head) ++ super.getNextStatements(obj, labels)
  }

  override def getLabels(obj: NodePath): Map[Any, NodePath] = {
    val nextMap = obj.asInstanceOf[SequenceElement].getNext.fold[Map[Any, NodePath]](Map.empty)(
      next => Map(getNextLabel(IfThenDelta.getThenStatements(obj).last) -> next)
    ) //TODO this will not work for an if-if nesting. Should generate a next label for each statement. But this also requires labels referencing other labels.
    nextMap ++ super.getLabels(obj)
  }

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, statement: NodePath, parentScope: Scope): Unit = {
    val bodyScope = builder.newScope(Some(parentScope), "thenScope")
    val body = IfThenDelta.getThenStatements(statement)
    BlockDelta.collectConstraints(compilation, builder, body, bodyScope)
    val condition = IfThenDelta.getCondition(statement)
    ExpressionSkeleton.constraints(compilation, builder, condition, BooleanTypeDelta.constraintType, parentScope)
  }

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {}
}
