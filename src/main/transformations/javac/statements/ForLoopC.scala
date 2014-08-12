package transformations.javac.statements

import core.grammar.seqr
import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.javac.expressions.ExpressionC

object ForLoopC extends GrammarTransformation {

  override def inject(state: TransformationState): Unit = {
    StatementC.getStatementToLines(state).put(ForLoopKey, forLoop => {
      val initializer = getInitializer(forLoop)
      val condition = getCondition(forLoop)
      val forBody = getBody(forLoop)
      val whileBody = forBody ++ Seq(getIncrement(forLoop))
      val _while = WhileC._while(condition, whileBody)

      val toInstructions = StatementC.getToInstructions(state)
      toInstructions(initializer) ++ toInstructions(_while) //TODO maybe translate to statements instead of bytecode.
    })
    super.inject(state)
  }

  def getInitializer(forLoop: MetaObject) = forLoop(InitializerKey).asInstanceOf[MetaObject]

  def getCondition(forLoop: MetaObject) = forLoop(ConditionKey).asInstanceOf[MetaObject]

  def getIncrement(forLoop: MetaObject) = forLoop(IncrementKey).asInstanceOf[MetaObject]

  def getBody(forLoop: MetaObject) = forLoop(BodyKey).asInstanceOf[Seq[MetaObject]]

  override def dependencies: Set[Contract] = Set(WhileC)

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val statementGrammar = grammars.find(StatementC.StatementGrammar)
    val expressionGrammar = grammars.find(ExpressionC.ExpressionGrammar)
    val blockGrammar = grammars.find(BlockC.BlockGrammar)
    val forLoopGrammar = "for" ~> ("(" ~> statementGrammar ~ (expressionGrammar <~ ";") ~ expressionGrammar <~ ")") ~ blockGrammar ^^ { case initializer seqr condition seqr increment seqr body => forLoop(initializer.asInstanceOf[MetaObject],
      condition.asInstanceOf[MetaObject], increment.asInstanceOf[MetaObject], body.asInstanceOf[Seq[MetaObject]])
    }
    statementGrammar.inner = statementGrammar.inner | forLoopGrammar
  }

  def forLoop(initializer: MetaObject, condition: MetaObject, increment: MetaObject, body: Seq[MetaObject]) =
    new MetaObject(ForLoopKey, InitializerKey -> initializer, ConditionKey -> condition, IncrementKey -> increment, BodyKey -> body)

  object ForLoopKey

  object InitializerKey

  object ConditionKey

  object IncrementKey

  object BodyKey

}
