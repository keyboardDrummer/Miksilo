package transformations.javac.statements
import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.node._
import core.particles.path.{Path, PathRoot, SequenceElement}
import transformations.javac.expressions.ExpressionSkeleton
import transformations.javac.expressions.ExpressionSkeleton.Expression
import transformations.javac.statements.StatementSkeleton.Statement

object ForLoopC extends DeltaWithPhase with DeltaWithGrammar {

  implicit class ForLoop[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def initializer: Statement[T] = node(Initializer).asInstanceOf[T]
    def initializer_=(value: T) = node(Initializer) = value

    def condition: Expression = node(Condition).asInstanceOf[Node]
    def condition_=(value: T) = node(Condition) = value

    def increment: Expression = node(Increment).asInstanceOf[Node]
    def increment_=(value: Node) = node(Increment) = value

    def body: Seq[Node] = node(Body).asInstanceOf[Seq[Node]]
    def body_=(value: Node) = node(Body) = value
  }

  override def dependencies: Set[Contract] = Set(WhileC)

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    import grammars._
    val statementGrammar = find(StatementSkeleton.StatementGrammar)
    val expressionGrammar = find(ExpressionSkeleton.ExpressionGrammar)
    val blockGrammar = find(BlockDelta.Grammar)
    val forLoopGrammar = "for" ~> (statementGrammar.as(Initializer) ~
      expressionGrammar.as(Condition) ~< ";" ~
      expressionGrammar.as(Increment)).inParenthesis %
      blockGrammar.as(Body) asNode ForLoopType
    statementGrammar.inner = statementGrammar.inner | forLoopGrammar
  }

  def forLoop(initializer: Node, condition: Node, increment: Node, body: Seq[Node]) =
    new Node(ForLoopType, Initializer -> initializer, Condition -> condition, Increment -> increment, Body -> body)

  object ForLoopType extends NodeClass

  object Initializer extends NodeField

  object Condition extends NodeField

  object Increment extends NodeField

  object Body extends NodeField

  override def transform(program: Node, state: Compilation): Unit = {
    PathRoot(program).visit(path => path.clazz match {
      case ForLoopType => transformForLoop(path)
      case _ =>
    })
  }
  
  def transformForLoop(forLoopPath: Path): Unit = {
    val forLoop: ForLoop[Node] = forLoopPath.current
    val whileBody = forLoop.body ++
      Seq(ExpressionAsStatementC.create(forLoop.increment))
    val _while = WhileC.create(forLoop.condition, whileBody)

    val newStatements = Seq[Node](forLoop.initializer, _while)
    forLoopPath.asInstanceOf[SequenceElement].replaceWith(newStatements)
  }

  override def description: String = "Enables using the non-iterator for loop."
}
