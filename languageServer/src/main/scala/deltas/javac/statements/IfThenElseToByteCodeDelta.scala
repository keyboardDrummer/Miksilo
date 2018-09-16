package deltas.javac.statements

import core.deltas.Contract
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.{NodePath, SequenceElement}
import core.language.node._
import core.language.{Compilation, Language}
import deltas.bytecode.ByteCodeMethodInfo
import deltas.bytecode.simpleBytecode.{InferredStackFrames, LabelDelta, LabelledLocations}
import deltas.javac.expressions.ToByteCodeSkeleton
import deltas.statement.IfThenDelta._
import deltas.statement.IfThenElseDelta._
import deltas.statement.{BlockDelta, IfThenElseDelta}

object IfThenElseToByteCodeDelta extends ByteCodeStatementInstance {

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
    val thenBody = getThenStatement(ifThenElse)

    val jumpToElseIfFalse = LabelledLocations.ifZero(elseLabelName)
    val toInstructionsExpr = ToByteCodeSkeleton.getToInstructions(compilation)
    val toInstructionsStatement = ByteCodeStatementSkeleton.getToInstructions(compilation)
    toInstructionsExpr(condition) ++
      Seq(jumpToElseIfFalse) ++
      toInstructionsStatement(thenBody) ++
      Seq(LabelledLocations.goTo(endLabelName), elseLabel) ++
      toInstructionsStatement(getElseStatements(ifThenElse)) ++
      Seq(endLabel)
  }

  def shape: NodeShape = IfThenElseDelta.Shape

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = { }

  override def getNextStatements(language: Language, obj: NodePath, labels: Map[Any, NodePath]): Set[NodePath] =
  {
    val thenStatement = getThenStatement(obj)
    val elseStatement = getElseStatements(obj)
    val thenNextStatements = ByteCodeStatementSkeleton.getInstance(language, thenStatement).getNextStatements(language, thenStatement, labels)
    val elseNextStatements = ByteCodeStatementSkeleton.getInstance(language, elseStatement).getNextStatements(language, elseStatement, labels)
    thenNextStatements ++ elseNextStatements ++ super.getNextStatements(language, obj, labels)
  }

  override def getLabels(language: Language, obj: NodePath): Map[Any, NodePath] = {
    val next = obj.asInstanceOf[SequenceElement].next //TODO this will not work for an if-if nesting. Should generate a next label for each statement. But this also requires labels referencing other labels.
    val thenStatement = getThenStatement(obj)
    val elseStatement = getElseStatements(obj)
    val thenNextLabel = ByteCodeStatementSkeleton.getInstance(language, thenStatement).getNextLabel(thenStatement)
    val elseNextLabel = ByteCodeStatementSkeleton.getInstance(language, elseStatement).getNextLabel(elseStatement)
    Map(thenNextLabel -> next, elseNextLabel -> next) ++ super.getLabels(language, obj)
  }
}
