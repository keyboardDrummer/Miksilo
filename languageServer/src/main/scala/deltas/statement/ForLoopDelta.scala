package deltas.statement

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.{NodePath, NodeSequenceElement, PathRoot}
import core.language.node._
import core.language.{Compilation, Language}
import deltas.expression.ExpressionDelta
import deltas.expression.ExpressionDelta.Expression
import deltas.javac.statements.ExpressionAsStatementDelta

object ForLoopDelta extends DeltaWithPhase with DeltaWithGrammar {

  override def description: String = "Enables using the non-iterator for loop."

  override def dependencies: Set[Contract] = Set(WhileLoopDelta, BlockAsStatementDelta)

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._

    val statementGrammar = find(StatementDelta.Grammar)
    val expressionGrammar = find(ExpressionDelta.FirstPrecedenceGrammar)
    val blockGrammar = find(BlockDelta.BlockOrStatementGrammar)
    val forLoopGrammar = "for" ~> (statementGrammar.as(Initializer) ~
      expressionGrammar.as(Condition) ~< ";" ~
      expressionGrammar.as(Increment)).inParenthesis %
      blockGrammar.as(Body) asNode Shape
    statementGrammar.addAlternative(forLoopGrammar)
  }

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    PathRoot(program).visitShape(Shape, path => transformForLoop(path))
  }

  def transformForLoop(forLoopPath: NodePath): Unit = {
    val forLoop: ForLoop[Node] = forLoopPath.current
    val whileBody = Seq(forLoop.body, ExpressionAsStatementDelta.create(forLoop.increment))
    val _while = WhileLoopDelta.create(forLoop.condition, BlockDelta.neww(whileBody))

    val newStatements = Seq[Node](forLoop.initializer, _while)
    val block = BlockDelta.Shape.create(BlockDelta.Statements -> newStatements)
    forLoopPath.asInstanceOf[NodeSequenceElement].replaceWith(block)
  }

  implicit class ForLoop[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def initializer: T = node(Initializer).asInstanceOf[T]
    def initializer_=(value: T): Unit = node(Initializer) = value

    def condition: Expression = node(Condition).asInstanceOf[Node]
    def condition_=(value: T): Unit = node(Condition) = value

    def increment: Expression = node(Increment).asInstanceOf[Node]
    def increment_=(value: Node): Unit = node(Increment) = value

    def body: T = node(Body).asInstanceOf[T]
    def body_=(value: Node): Unit = node(Body) = value
  }

  def forLoop(initializer: Node, condition: Node, increment: Node, body: Node) =
    new Node(Shape, Initializer -> initializer, Condition -> condition, Increment -> increment, Body -> body)

  object Shape extends NodeShape

  object Initializer extends NodeField

  object Condition extends NodeField

  object Increment extends NodeField

  object Body extends NodeField
}
