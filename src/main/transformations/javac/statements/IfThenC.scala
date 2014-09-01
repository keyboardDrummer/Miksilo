package transformations.javac.statements

import core.transformation.grammars.GrammarCatalogue
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.bytecode.LabelledTargets
import transformations.bytecode.simpleBytecode.InferredStackFrames
import transformations.javac.expressions.ExpressionC

object IfThenC extends StatementInstance {

  object IfThenKey

  object ConditionKey

  object ThenKey

  override val key: AnyRef = IfThenKey

  override def dependencies: Set[Contract] = super.dependencies ++ Set(BlockC)

  override def toByteCode(ifThen: MetaObject, state: TransformationState): Seq[MetaObject] = {
    val condition = ifThen(ConditionKey).asInstanceOf[MetaObject]
    val endLabelName = state.getUniqueLabel("end")
    val end = InferredStackFrames.label(endLabelName)
    val body = ifThen(ThenKey).asInstanceOf[Seq[MetaObject]]

    val conditionalBranch = LabelledTargets.ifZero(endLabelName)
    val toInstructionsExpr = ExpressionC.getToInstructions(state)
    val toInstructionsStatement = StatementC.getToInstructions(state)
    toInstructionsExpr(condition) ++
      Seq(conditionalBranch) ++
      body.flatMap(toInstructionsStatement) ++
      Seq(end)
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val statementGrammar = grammars.find(StatementC.StatementGrammar)
    val expressionGrammar = grammars.find(ExpressionC.ExpressionGrammar)
    val bodyGrammar = grammars.find(BlockC.BlockGrammar) | (statementGrammar ^^ (statement => Seq(statement), x => Some(x.asInstanceOf[Seq[Any]](0))))
    val ifThenGrammar = "if" ~> ("(" ~> expressionGrammar <~ ")") ~ bodyGrammar ^^ parseMap(IfThenKey, ConditionKey, ThenKey)
    statementGrammar.orToInner(ifThenGrammar)
  }
}
