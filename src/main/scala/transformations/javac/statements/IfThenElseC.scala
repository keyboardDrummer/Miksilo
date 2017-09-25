package transformations.javac.statements

import core.particles.{CompilationState, FromMap}
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node, NodeLike}
import core.particles.path.{Path, SequenceElement}
import transformations.bytecode.ByteCodeMethodInfo
import transformations.bytecode.additions.LabelledLocations
import transformations.bytecode.simpleBytecode.InferredStackFrames
import transformations.javac.expressions.ExpressionSkeleton
import transformations.javac.statements.IfThenC._

object IfThenElseC extends StatementInstance {

  override def toByteCode(ifThenElse: Path, state: CompilationState): Seq[Node] = {
    val condition = getCondition(ifThenElse)
    val methodInfo = ifThenElse.findAncestorClass(ByteCodeMethodInfo.MethodInfoKey)
    val endLabelName = LabelledLocations.getUniqueLabel("end", methodInfo, state)
    val elseLabelName = LabelledLocations.getUniqueLabel("else", methodInfo, state)
    val endLabel = InferredStackFrames.label(endLabelName)
    val elseLabel = InferredStackFrames.label(elseLabelName)
    val thenBody = getThenStatements(ifThenElse)

    val jumpToElseIfFalse = LabelledLocations.ifZero(elseLabelName)
    val toInstructionsExpr = ExpressionSkeleton.getToInstructions(state)
    val toInstructionsStatement = StatementSkeleton.getToInstructions(state)
    toInstructionsExpr(condition) ++
      Seq(jumpToElseIfFalse) ++
      thenBody.flatMap(toInstructionsStatement) ++
      Seq(LabelledLocations.goTo(endLabelName), elseLabel) ++
      getElseStatements(ifThenElse).flatMap(toInstructionsStatement) ++
      Seq(endLabel)
  }

  object ElseKey extends Key

  override def transformGrammars(grammars: GrammarCatalogue, state: CompilationState): Unit = {
    val statementGrammar = grammars.find(StatementSkeleton.StatementGrammar)
    val bodyGrammar = grammars.find(BlockC.BlockOrStatementGrammar)
    val ifThenGrammar = grammars.find(IfThenC)
    val ifThenElseGrammar = ifThenGrammar ~ ("else" ~> bodyGrammar) asNode(key, FromMap, ElseKey)
    statementGrammar.addOption(ifThenElseGrammar)
  }

  def getElseStatements[T <: NodeLike](ifThen: T): Seq[T] = {
    ifThen(ElseKey).asInstanceOf[Seq[T]]
  }

  override def getNextStatements(obj: Path, labels: Map[Any, Path]): Set[Path] =
  {
    Set(getThenStatements(obj).head, getElseStatements(obj).head) ++ super.getNextStatements(obj, labels)
  }

  override def getLabels(obj: Path): Map[Any, Path] = {
    val next = obj.asInstanceOf[SequenceElement].next //TODO this will not work for an if-if nesting. Should generate a next label for each statement. But this also requires labels referencing other labels.
    Map(IfThenC.getNextLabel(getThenStatements(obj).last) -> next, IfThenC.getNextLabel(getElseStatements(obj).last) -> next) ++
      super.getLabels(obj)
  }

  override def description: String = "Enables using the if-then-else construct."
}
