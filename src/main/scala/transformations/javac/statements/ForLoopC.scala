package transformations.javac.statements
import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.node.Node
import core.particles.path.{Path, PathRoot, SequenceSelection}
import transformations.javac.expressions.ExpressionSkeleton

object ForLoopC extends ParticleWithPhase with ParticleWithGrammar {

  implicit class ForLoop(forLoop: Node) {
    def initializer = forLoop(InitializerKey).asInstanceOf[Node]
    def initializer_=(value: Node) = forLoop(InitializerKey) = value

    def condition = forLoop(ConditionKey).asInstanceOf[Node]
    def condition_=(value: Node) = forLoop(ConditionKey) = value

    def increment = forLoop(IncrementKey).asInstanceOf[Node]
    def increment_=(value: Node) = forLoop(IncrementKey) = value

    def body = forLoop(BodyKey).asInstanceOf[Seq[Node]]
    def body_=(value: Node) = forLoop(BodyKey) = value
  }

  override def dependencies: Set[Contract] = Set(WhileC)

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val statementGrammar = grammars.find(StatementSkeleton.StatementGrammar)
    val expressionGrammar = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    val blockGrammar = grammars.find(BlockC.BlockGrammar)
    val forLoopGrammar = "for" ~> ("(" ~> statementGrammar ~ (expressionGrammar <~ ";") ~ expressionGrammar <~ ")") % blockGrammar ^^
      parseMap(ForLoopKey, InitializerKey, ConditionKey, IncrementKey, BodyKey)
    statementGrammar.inner = statementGrammar.inner | forLoopGrammar
  }

  def forLoop(initializer: Node, condition: Node, increment: Node, body: Seq[Node]) =
    new Node(ForLoopKey, InitializerKey -> initializer, ConditionKey -> condition, IncrementKey -> increment, BodyKey -> body)

  object ForLoopKey

  object InitializerKey

  object ConditionKey

  object IncrementKey

  object BodyKey

  override def transform(program: Node, state: CompilationState): Unit = {
    PathRoot(program).foreach(path => path.clazz match {
      case ForLoopKey => transformForLoop(path, state)
      case _ =>
    })
  }
  
  def transformForLoop(forLoopPath: Path, state: CompilationState): Unit = {
    val forLoop: ForLoop = forLoopPath.current
    val whileBody = forLoop.body ++
      Seq(ExpressionAsStatementC.create(forLoop.increment))
    val _while = WhileC._while(forLoop.condition, whileBody)

    val newStatements = Seq(forLoop.initializer, _while)
    forLoopPath.asInstanceOf[SequenceSelection].replaceWith(newStatements)
  }

  override def description: String = "Enables using the non-iterator for loop."
}
