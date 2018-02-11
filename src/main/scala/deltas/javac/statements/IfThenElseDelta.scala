package deltas.javac.statements

import core.deltas.{Compilation, Contract, NodeGrammar}
import core.deltas.grammars.LanguageGrammars
import core.deltas.node._
import core.deltas.path.{ChildPath, NodePath, SequenceElement}
import core.language.Language
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import deltas.bytecode.ByteCodeMethodInfo
import deltas.bytecode.simpleBytecode.{InferredStackFrames, LabelDelta, LabelledLocations}
import deltas.javac.expressions.ExpressionSkeleton
import deltas.javac.statements.IfThenDelta._

object IfThenElseDelta extends StatementInstance {

  override def description: String = "Enables using the if-then-else construct."

  override def dependencies: Set[Contract] = super.dependencies ++ Set(IfThenDelta, LabelledLocations, InferredStackFrames, BlockDelta)

  override def toByteCode(ifThenElse: NodePath, compilation: Compilation): Seq[Node] = {
    val condition = getCondition(ifThenElse)
    val methodInfo = ifThenElse.findAncestorShape(ByteCodeMethodInfo.MethodInfoKey)
    val endLabelName = LabelDelta.getUniqueLabel("end", methodInfo)
    val elseLabelName = LabelDelta.getUniqueLabel("else", methodInfo)
    val endLabel = InferredStackFrames.label(endLabelName)
    val elseLabel = InferredStackFrames.label(elseLabelName)
    val thenBody = getThenStatements(ifThenElse)

    val jumpToElseIfFalse = LabelledLocations.ifZero(elseLabelName)
    val toInstructionsExpr = ExpressionSkeleton.getToInstructions(compilation)
    val toInstructionsStatement = StatementSkeleton.getToInstructions(compilation)
    toInstructionsExpr(condition) ++
      Seq(jumpToElseIfFalse) ++
      thenBody.flatMap(toInstructionsStatement) ++
      Seq(LabelledLocations.goTo(endLabelName), elseLabel) ++
      getElseStatements(ifThenElse).flatMap(toInstructionsStatement) ++
      Seq(endLabel)
  }

  def shape = Shape
  object Shape extends NodeShape
  object ElseKey extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val statementGrammar = find(StatementSkeleton.StatementGrammar)
    val bodyGrammar = find(BlockDelta.BlockOrStatementGrammar)
    val ifThenGrammar = find(IfThenDelta.shape)
    val ifThenElseGrammar = ifThenGrammar.inner.asInstanceOf[NodeGrammar].inner ~ ("else" ~> bodyGrammar.as(ElseKey)) asNode shape
    statementGrammar.addOption(ifThenElseGrammar)
  }

  def getElseStatements[T <: NodeLike](ifThen: T): Seq[T] = {
    ifThen(ElseKey).asInstanceOf[Seq[T]]
  }

  override def getNextStatements(obj: NodePath, labels: Map[Any, NodePath]): Set[NodePath] =
  {
    Set(getThenStatements(obj).head, getElseStatements(obj).head) ++ super.getNextStatements(obj, labels)
  }

  override def getLabels(obj: NodePath): Map[Any, NodePath] = {
    val next = obj.asInstanceOf[SequenceElement].next //TODO this will not work for an if-if nesting. Should generate a next label for each statement. But this also requires labels referencing other labels.
    Map(IfThenDelta.getNextLabel(getThenStatements(obj).last) -> next, IfThenDelta.getNextLabel(getElseStatements(obj).last) -> next) ++
      super.getLabels(obj)
  }

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, statement: ChildPath, parentScope: Scope): Unit = {
    IfThenDelta.constraints(compilation, builder, statement, parentScope)
    val elseBodyScope = builder.newScope(Some(parentScope), "elseScope")
    val elseBody = getElseStatements(statement)
    BlockDelta.collectConstraints(compilation, builder, elseBody, elseBodyScope)
  }
}
