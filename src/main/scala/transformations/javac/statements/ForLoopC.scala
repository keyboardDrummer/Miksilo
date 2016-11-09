package transformations.javac.statements
import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeWrapper}
import core.particles.path.{Path, PathRoot, SequenceElement}
import transformations.javac.expressions.ExpressionSkeleton
import transformations.javac.expressions.ExpressionSkeleton.Expression
import transformations.javac.statements.StatementSkeleton.Statement

object ForLoopC extends DeltaWithPhase with DeltaWithGrammar {

  implicit class ForLoop(val node: Node)
    extends AnyVal with NodeWrapper {
    def initializer: Statement = node(Initializer).asInstanceOf[Node]
    def initializer_=(value: Node) = node(Initializer) = value

    def condition: Expression = node(Condition).asInstanceOf[Node]
    def condition_=(value: Node) = node(Condition) = value

    def increment: Expression = node(Increment).asInstanceOf[Node]
    def increment_=(value: Node) = node(Increment) = value

    def body: Seq[Node] = node(Body).asInstanceOf[Seq[Node]]
    def body_=(value: Node) = node(Body) = value
  }

  override def dependencies: Set[Contract] = Set(WhileC)

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val statementGrammar = grammars.find(StatementSkeleton.StatementGrammar)
    val expressionGrammar = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    val blockGrammar = grammars.find(BlockC.BlockGrammar)
    val forLoopGrammar = "for" ~> ("(" ~> statementGrammar ~ (expressionGrammar <~ ";") ~ expressionGrammar <~ ")") % blockGrammar ^^
      parseMap(ForLoopKey, Initializer, Condition, Increment, Body)
    statementGrammar.inner = statementGrammar.inner | forLoopGrammar
  }

  def forLoop(initializer: Node, condition: Node, increment: Node, body: Seq[Node]) =
    new Node(ForLoopKey, Initializer -> initializer, Condition -> condition, Increment -> increment, Body -> body)

  object ForLoopKey

  object Initializer

  object Condition

  object Increment

  object Body

  override def transform(program: Node, state: CompilationState): Unit = {
    PathRoot(program).visit(path => path.clazz match {
      case ForLoopKey => transformForLoop(path, state)
      case _ =>
    })
  }
  
  def transformForLoop(forLoopPath: Path, state: CompilationState): Unit = {
    val forLoop: ForLoop = forLoopPath.current
    val whileBody = forLoop.body ++ Seq(ExpressionAsStatementC.create(forLoop.increment))
    val _while = WhileC.create(forLoop.condition, whileBody)

    val newStatements = Seq[Node](forLoop.initializer, _while)
    forLoopPath.asInstanceOf[SequenceElement].replaceWith(newStatements)
  }

  override def description: String = "Enables using the non-iterator for loop."
}
