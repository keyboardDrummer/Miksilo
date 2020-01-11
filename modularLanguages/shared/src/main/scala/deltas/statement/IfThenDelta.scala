package deltas.statement

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node._
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import deltas.ConstraintSkeleton
import deltas.expression.ExpressionDelta
import deltas.javac.types.BooleanTypeDelta

object IfThenDelta extends DeltaWithGrammar with StatementInstance {

  override def description: String = "Enables using the if-then (no else) construct."

  def neww(condition: Node, thenBody: Node): Node = Shape.create(Condition -> condition, Then -> thenBody)

  object Shape extends NodeShape

  object Condition extends NodeField

  object Then extends NodeField

  override def dependencies: Set[Contract] = super.dependencies ++ Set(BlockDelta)

  implicit class IfThen[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def condition: T = node(Condition).asInstanceOf[T]
    def thenStatement: T = node(Then).asInstanceOf[T]
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val statementGrammar = find(StatementDelta.Grammar)
    val expressionGrammar = find(ExpressionDelta.FirstPrecedenceGrammar)
    val bodyGrammar = find(BlockDelta.BlockOrStatementGrammar)
    val ifThenGrammar = create(Shape, ("if" ~> ("(" ~> expressionGrammar.as(Condition) ~< ")") % bodyGrammar.as(Then)).
      asNode(Shape))
    statementGrammar.addAlternative(ifThenGrammar)
  }

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, statement: NodePath, parentScope: Scope): Unit = {
    val ifThen: IfThen[NodePath] = statement
    ConstraintSkeleton.constraints(compilation, builder, ifThen.thenStatement, parentScope)
    ExpressionDelta.addConstraints(compilation, builder, ifThen.condition, BooleanTypeDelta.constraintType, parentScope)
  }

  override def shape: NodeShape = Shape

  override def getControlFlowGraph(language: Language, statement: NodePath, labels: Map[Any, NodePath]): ControlFlowGraph = {
    val thenStatement = statement.thenStatement
    val thenGraph = ControlFlowGraph.getControlFlowGraph(language, thenStatement, labels)
    val conditionGraph = ControlFlowGraph.singleton(statement)
    val trueGraph = conditionGraph.sequence(thenGraph)
    val elseGraph = conditionGraph
    trueGraph.parallel(elseGraph)
  }
}
