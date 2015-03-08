package transformations.javac.statements
import core.particles.grammars.GrammarCatalogue
import core.particles.{Contract, MetaObject, CompilationState}
import transformations.javac.expressions.ExpressionSkeleton

object ForLoopC extends StatementInstance {

  def getInitializer(forLoop: MetaObject) = forLoop(InitializerKey).asInstanceOf[MetaObject]

  def getCondition(forLoop: MetaObject) = forLoop(ConditionKey).asInstanceOf[MetaObject]

  def getIncrement(forLoop: MetaObject) = forLoop(IncrementKey).asInstanceOf[MetaObject]

  def getBody(forLoop: MetaObject) = forLoop(BodyKey).asInstanceOf[Seq[MetaObject]]

  override def dependencies: Set[Contract] = Set(WhileC)

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val statementGrammar = grammars.find(StatementSkeleton.StatementGrammar)
    val expressionGrammar = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    val blockGrammar = grammars.find(BlockC.BlockGrammar)
    val forLoopGrammar = "for" ~> ("(" ~> statementGrammar ~ (expressionGrammar <~ ";") ~ expressionGrammar <~ ")") % blockGrammar ^^
      parseMap(ForLoopKey, InitializerKey, ConditionKey, IncrementKey, BodyKey)
    statementGrammar.inner = statementGrammar.inner | forLoopGrammar
  }

  def forLoop(initializer: MetaObject, condition: MetaObject, increment: MetaObject, body: Seq[MetaObject]) =
    new MetaObject(ForLoopKey, InitializerKey -> initializer, ConditionKey -> condition, IncrementKey -> increment, BodyKey -> body)

  object ForLoopKey

  object InitializerKey

  object ConditionKey

  object IncrementKey

  object BodyKey

  override val key: AnyRef = ForLoopKey

  override def toByteCode(forLoop: MetaObject, state: CompilationState): Seq[MetaObject] = {
    val initializer = getInitializer(forLoop)
    val condition = getCondition(forLoop)
    val forBody = getBody(forLoop)
    val whileBody = forBody ++ Seq(ExpressionAsStatementC.asStatement(getIncrement(forLoop)))
    val _while = WhileC._while(condition, whileBody)

    val toInstructions = StatementSkeleton.getToInstructions(state)
    toInstructions(initializer) ++ toInstructions(_while) //TODO maybe translate to statements instead of bytecode.
  }

  override def description: String = "Enables using the non-iterator for loop."
}
