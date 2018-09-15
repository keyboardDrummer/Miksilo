package deltas.javac.statements

import core.deltas.Contract
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.{NodePath, SequenceElement}
import core.language.node._
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import deltas.bytecode.ByteCodeMethodInfo
import deltas.bytecode.simpleBytecode.{InferredStackFrames, LabelDelta, LabelledLocations}
import deltas.javac.expressions.ToByteCodeSkeleton
import deltas.statement.IfThenDelta._
import deltas.statement.{BlockDelta, IfThenElseDelta}
import deltas.statement.IfThenElseDelta._

object IfThenElseToByteCodeDelta extends StatementInstance {

  override def description: String = "Enables using the if-then-else construct."

  override def dependencies: Set[Contract] = super.dependencies ++
    Set(IfThenElseDelta, IfThenToByteCodeDelta, LabelledLocations, InferredStackFrames, BlockDelta)

  override def toByteCode(ifThenElse: NodePath, compilation: Compilation): Seq[Node] = {
    val condition = getCondition(ifThenElse)
    val methodInfo = ifThenElse.findAncestorShape(ByteCodeMethodInfo.Shape)
    val endLabelName = LabelDelta.getUniqueLabel("end", methodInfo)
    val elseLabelName = LabelDelta.getUniqueLabel("else", methodInfo)
    val endLabel = InferredStackFrames.label(endLabelName)
    val elseLabel = InferredStackFrames.label(elseLabelName)
    val thenBody = getThenStatements(ifThenElse)

    val jumpToElseIfFalse = LabelledLocations.ifZero(elseLabelName)
    val toInstructionsExpr = ToByteCodeSkeleton.getToInstructions(compilation)
    val toInstructionsStatement = ByteCodeStatementSkeleton.getToInstructions(compilation)
    toInstructionsExpr(condition) ++
      Seq(jumpToElseIfFalse) ++
      thenBody.flatMap(toInstructionsStatement) ++
      Seq(LabelledLocations.goTo(endLabelName), elseLabel) ++
      getElseStatements(ifThenElse).flatMap(toInstructionsStatement) ++
      Seq(endLabel)
  }

  def shape: NodeShape = IfThenElseDelta.Shape

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = { }

  override def getNextStatements(obj: NodePath, labels: Map[Any, NodePath]): Set[NodePath] =
  {
    Set(getThenStatements(obj).head, getElseStatements(obj).head) ++ super.getNextStatements(obj, labels)
  }

  override def getLabels(obj: NodePath): Map[Any, NodePath] = {
    val next = obj.asInstanceOf[SequenceElement].next //TODO this will not work for an if-if nesting. Should generate a next label for each statement. But this also requires labels referencing other labels.
    Map(IfThenToByteCodeDelta.getNextLabel(getThenStatements(obj).last) -> next, IfThenToByteCodeDelta.getNextLabel(getElseStatements(obj).last) -> next) ++
      super.getLabels(obj)
  }

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, statement: NodePath, parentScope: Scope): Unit = {
    IfThenToByteCodeDelta.constraints(compilation, builder, statement, parentScope)
    val elseBodyScope = builder.newScope(Some(parentScope), "elseScope")
    val elseBody = getElseStatements(statement)
    BlockDelta.collectConstraints(compilation, builder, elseBody, elseBodyScope)
  }
}
