package deltas.javac.statements
import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.node._
import core.deltas.path.{Path, PathRoot, SequenceElement}
import core.language.Language
import deltas.javac.expressions.ExpressionSkeleton
import deltas.javac.expressions.ExpressionSkeleton.Expression
import deltas.javac.statements.StatementSkeleton.Statement

object ForLoopDelta extends DeltaWithPhase with DeltaWithGrammar {

  override def description: String = "Enables using the non-iterator for loop."

  override def dependencies: Set[Contract] = Set(WhileLoopDelta)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val statementGrammar = find(StatementSkeleton.StatementGrammar)
    val expressionGrammar = find(ExpressionSkeleton.ExpressionGrammar)
    val blockGrammar = find(BlockDelta.Grammar)
    val forLoopGrammar = "for" ~> (statementGrammar.as(Initializer) ~
      expressionGrammar.as(Condition) ~< ";" ~
      expressionGrammar.as(Increment)).inParenthesis %
      blockGrammar.as(Body) asNode Shape
    statementGrammar.addOption(forLoopGrammar)
  }

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    PathRoot(program).visitShape(Shape, path => transformForLoop(path))
  }

  def transformForLoop(forLoopPath: Path): Unit = {
    val forLoop: ForLoop[Node] = forLoopPath.current
    val whileBody = forLoop.body ++
      Seq(ExpressionAsStatementDelta.create(forLoop.increment))
    val _while = WhileLoopDelta.create(forLoop.condition, whileBody)

    val newStatements = Seq[Node](forLoop.initializer, _while)
    forLoopPath.asInstanceOf[SequenceElement].replaceWith(newStatements)
  }

  implicit class ForLoop[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def initializer: Statement[T] = node(Initializer).asInstanceOf[T]
    def initializer_=(value: T): Unit = node(Initializer) = value

    def condition: Expression = node(Condition).asInstanceOf[Node]
    def condition_=(value: T): Unit = node(Condition) = value

    def increment: Expression = node(Increment).asInstanceOf[Node]
    def increment_=(value: Node): Unit = node(Increment) = value

    def body: Seq[Node] = node(Body).asInstanceOf[Seq[Node]]
    def body_=(value: Node): Unit = node(Body) = value
  }

  def forLoop(initializer: Node, condition: Node, increment: Node, body: Seq[Node]) =
    new Node(Shape, Initializer -> initializer, Condition -> condition, Increment -> increment, Body -> body)

  object Shape extends NodeShape

  object Initializer extends NodeField

  object Condition extends NodeField

  object Increment extends NodeField

  object Body extends NodeField
}
