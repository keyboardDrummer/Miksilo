package transformations.javac.statements


import core.transformation.grammars.GrammarCatalogue
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.bytecode.LabelledTargets
import transformations.bytecode.simpleBytecode.InferredStackFrames
import transformations.javac.expressions.ExpressionC

object WhileC extends StatementInstance {

  override val key: AnyRef = WhileKey

  override def toByteCode(_while: MetaObject, state: TransformationState): Seq[MetaObject] = {
    val conditionInstructions = ExpressionC.getToInstructions(state)(getCondition(_while))
    val body = getBody(_while)
    val bodyInstructions = body.flatMap(statement => StatementC.getToInstructions(state)(statement))

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
    val statementGrammar = grammars.find(StatementC.StatementGrammar)
    val expressionGrammar = grammars.find(ExpressionC.ExpressionGrammar)
    val blockGrammar = grammars.find(BlockC.BlockGrammar)
    val whileGrammar = "while" ~> ("(" ~> expressionGrammar <~ ")") ~ blockGrammar ^^
      parseMap(WhileKey, WhileCondition, WhileBody)
    statementGrammar.orToInner(whileGrammar)
  }

  def _while(condition: MetaObject, body: Seq[MetaObject]) = new MetaObject(WhileKey, WhileCondition -> condition, WhileBody -> body)


  object WhileKey

  object WhileCondition

  object WhileBody

}
