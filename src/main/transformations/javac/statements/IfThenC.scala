package transformations.javac.statements

import core.particles.grammars.GrammarCatalogue
import core.particles._
import transformations.bytecode.additions.LabelledTargets
import transformations.bytecode.simpleBytecode.InferredStackFrames
import transformations.javac.expressions.ExpressionSkeleton

object IfThenC extends StatementInstance {

  object IfThenKey

  object ConditionKey

  object ThenKey

  override val key: AnyRef = IfThenKey

  override def dependencies: Set[Contract] = super.dependencies ++ Set(BlockC)

  override def toByteCode(ifThen: Origin, state: CompilationState): Seq[MetaObject] = {
    val condition = getCondition(ifThen)
    val endLabelName = state.getUniqueLabel("end")
    val end = InferredStackFrames.label(endLabelName)
    val body = getThenStatements(ifThen)

    val conditionalBranch = LabelledTargets.ifZero(endLabelName)
    val toInstructionsExpr = ExpressionSkeleton.getToInstructions(state)
    val toInstructionsStatement = StatementSkeleton.getToInstructions(state)
    toInstructionsExpr(condition) ++
      Seq(conditionalBranch) ++
      body.flatMap(toInstructionsStatement) ++
      Seq(end)
  }

  def getCondition[T <: MetaLike](ifThen: T): T = {
    ifThen(ConditionKey).asInstanceOf[T]
  }

  def getThenStatements[T <: MetaLike](ifThen: T): Seq[T] = {
    ifThen(ThenKey).asInstanceOf[Seq[T]]
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val statementGrammar = grammars.find(StatementSkeleton.StatementGrammar)
    val expressionGrammar = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    val bodyGrammar = grammars.find(BlockC.BlockGrammar) | (statementGrammar ^^ (statement => Seq(statement), x => Some(x.asInstanceOf[Seq[Any]](0))))
    val ifThenGrammar = "if" ~> ("(" ~> expressionGrammar <~ ")") ~ bodyGrammar ^^ parseMap(IfThenKey, ConditionKey, ThenKey)
    statementGrammar.addOption(ifThenGrammar)
  }

  override def description: String = "Enables using the if-then (no else) construct."

  override def getNextStatements(obj: Origin, labels: Map[Any, Origin]): Set[Origin] =
  {
    Set(getThenStatements(obj)(0)) ++ super.getNextStatements(obj, labels)
  }

  override def getLabels(obj: Origin): Map[Any, Origin] = {
    val next = obj.asInstanceOf[SequenceSelection].next //TODO this will not work for an if-if nesting. Should generate a next label for each statement. But this also requires labels referencing other labels.
    Map(IfThenC.getNextLabel(getThenStatements(obj).last) -> next) ++
      super.getLabels(obj)
  }
}
