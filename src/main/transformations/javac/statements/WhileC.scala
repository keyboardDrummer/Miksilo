package transformations.javac.statements


import core.transformation.grammars.GrammarCatalogue
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.bytecode.additions.LabelledTargets
import transformations.bytecode.simpleBytecode.InferredStackFrames
import transformations.javac.expressions.ExpressionSkeleton

object WhileC extends StatementInstance {

  override val key: AnyRef = WhileKey

  override def toByteCode(_while: MetaObject, state: TransformationState): Seq[MetaObject] = {
    val conditionInstructions = ExpressionSkeleton.getToInstructions(state)(getCondition(_while))
    val body = getBody(_while)
    val bodyInstructions = body.flatMap(statement => StatementSkeleton.getToInstructions(state)(statement))

    val startLabel = state.getUniqueLabel("start")
    val endLabel = state.getUniqueLabel("end")
    Seq(InferredStackFrames.label(startLabel)) ++
      conditionInstructions ++
      Seq(LabelledTargets.ifZero(endLabel)) ++
      bodyInstructions ++
      Seq(LabelledTargets.goTo(startLabel), InferredStackFrames.label(endLabel))
  }

  def getCondition(_while: MetaObject) = _while(WhileCondition).asInstanceOf[MetaObject]

  def getBody(_while: MetaObject) = _while(WhileBody).asInstanceOf[Seq[MetaObject]]

  override def dependencies: Set[Contract] = super.dependencies ++ Set(BlockC)

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val statementGrammar = grammars.find(StatementSkeleton.StatementGrammar)
    val expressionGrammar = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    val blockGrammar = grammars.find(BlockC.BlockGrammar)
    val whileGrammar = "while" ~> ("(" ~> expressionGrammar <~ ")") % blockGrammar ^^
      parseMap(WhileKey, WhileCondition, WhileBody)
    statementGrammar.addOption(whileGrammar)
  }

  def _while(condition: MetaObject, body: Seq[MetaObject]) = new MetaObject(WhileKey, WhileCondition -> condition, WhileBody -> body)


  object WhileKey

  object WhileCondition

  object WhileBody

  override def description: String = "Enables using the while construct."
}
