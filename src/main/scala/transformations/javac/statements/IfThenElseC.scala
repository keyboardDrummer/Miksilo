package transformations.javac.statements

import core.particles.CompilationState
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node, NodeLike}
import core.particles.path.{SequenceSelection, Path}
import transformations.bytecode.additions.LabelledTargets
import transformations.bytecode.simpleBytecode.InferredStackFrames
import transformations.javac.expressions.ExpressionSkeleton
import transformations.javac.statements.IfThenC._

object IfThenElseC extends StatementInstance {

  override def toByteCode(ifThenElse: Path, state: CompilationState): Seq[Node] = {
    val condition = getCondition(ifThenElse)
    val endLabelName = state.getUniqueLabel("end")
    val elseLabelName = state.getUniqueLabel("else")
    val endLabel = InferredStackFrames.label(endLabelName)
    val elseLabel = InferredStackFrames.label(elseLabelName)
    val thenBody = getThenStatements(ifThenElse)

    val jumpToElseIfFalse = LabelledTargets.ifZero(elseLabelName)
    val toInstructionsExpr = ExpressionSkeleton.getToInstructions(state)
    val toInstructionsStatement = StatementSkeleton.getToInstructions(state)
    toInstructionsExpr(condition) ++
      Seq(jumpToElseIfFalse) ++
      thenBody.flatMap(toInstructionsStatement) ++
      Seq(LabelledTargets.goTo(endLabelName), elseLabel) ++
      getElseStatements(ifThenElse).flatMap(toInstructionsStatement) ++
      Seq(endLabel)
  }

  object ElseKey extends Key

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val statementGrammar = grammars.find(StatementSkeleton.StatementGrammar)
    val bodyGrammar = grammars.find(BlockC.BlockOrStatementGrammar)
    val ifThenGrammar = grammars.find(IfThenC)
    val ifThenElseGrammar = ifThenGrammar ~ ("else" ~> bodyGrammar) ^^ parseMap(key, PartialSelf, ElseKey)
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
    val next = obj.asInstanceOf[SequenceSelection].next //TODO this will not work for an if-if nesting. Should generate a next label for each statement. But this also requires labels referencing other labels.
    Map(IfThenC.getNextLabel(getThenStatements(obj).last) -> next, IfThenC.getNextLabel(getElseStatements(obj).last) -> next) ++
      super.getLabels(obj)
  }

  override def description: String = "Enables using the if-then-else construct."
}
