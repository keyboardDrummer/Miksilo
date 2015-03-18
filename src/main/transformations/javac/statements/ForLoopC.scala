package transformations.javac.statements
import core.particles.grammars.GrammarCatalogue
import core.particles._
import transformations.javac.expressions.ExpressionSkeleton

object ForLoopC extends ParticleWithPhase with ParticleWithGrammar {

  def getInitializer[T <: MetaLike](forLoop: T) = forLoop(InitializerKey).asInstanceOf[T]

  def getCondition[T <: MetaLike](forLoop: T) = forLoop(ConditionKey).asInstanceOf[T]

  def getIncrement[T <: MetaLike](forLoop: T) = forLoop(IncrementKey).asInstanceOf[T]

  def getBody[T <: MetaLike](forLoop: T) = forLoop(BodyKey).asInstanceOf[Seq[T]]

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

  def transformForLoop(forLoop: Origin, state: CompilationState): Unit = {
    val initializer = getInitializer(forLoop)
    val condition = getCondition(forLoop)
    val forBody = getBody(forLoop.obj)
    val whileBody = forBody ++ Seq(ExpressionAsStatementC.asStatement(getIncrement(forLoop)))
    val _while = WhileC._while(condition.obj, whileBody)

    val newStatements = Seq(initializer.obj, _while)
    val originSequence = forLoop.asInstanceOf[SequenceSelection]
    originSequence.replaceWith(newStatements)
  }

  override def transform(program: MetaObject, state: CompilationState): Unit = {
    new Root(program).transform[Origin](obj => obj.clazz match {
      case ForLoopKey => transformForLoop(obj, state)
      case _ =>
    })
  }

  override def description: String = "Enables using the non-iterator for loop."
}
