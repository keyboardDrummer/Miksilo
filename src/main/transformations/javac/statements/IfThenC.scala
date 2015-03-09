package transformations.javac.statements

import core.particles.grammars.GrammarCatalogue
import core.particles.{CompilationState, Contract, MetaObject}
import transformations.bytecode.additions.LabelledTargets
import transformations.bytecode.simpleBytecode.InferredStackFrames
import transformations.javac.expressions.ExpressionSkeleton

object IfThenC extends StatementInstance {

  object IfThenKey

  object ConditionKey

  object ThenKey

  override val key: AnyRef = IfThenKey

  override def dependencies: Set[Contract] = super.dependencies ++ Set(BlockC)

  override def toByteCode(ifThen: MetaObject, state: CompilationState): Seq[MetaObject] = {
    val condition = ifThen(ConditionKey).asInstanceOf[MetaObject]
    val endLabelName = state.getUniqueLabel("end")
    val end = InferredStackFrames.label(endLabelName)
    val body = ifThen(ThenKey).asInstanceOf[Seq[MetaObject]]

    val conditionalBranch = LabelledTargets.ifZero(endLabelName)
    val toInstructionsExpr = ExpressionSkeleton.getToInstructions(state)
    val toInstructionsStatement = StatementSkeleton.getToInstructions(state)
    toInstructionsExpr(condition) ++
      Seq(conditionalBranch) ++
      body.flatMap(toInstructionsStatement) ++
      Seq(end)
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val statementGrammar = grammars.find(StatementSkeleton.StatementGrammar)
    val expressionGrammar = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    val bodyGrammar = grammars.find(BlockC.BlockGrammar) | (statementGrammar ^^ (statement => Seq(statement), x => Some(x.asInstanceOf[Seq[Any]](0))))
    val ifThenGrammar = "if" ~> ("(" ~> expressionGrammar <~ ")") ~ bodyGrammar ^^ parseMap(IfThenKey, ConditionKey, ThenKey)
    statementGrammar.addOption(ifThenGrammar)
  }

  override def description: String = "Enables using the if-then (no else) construct."
}
