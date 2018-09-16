package deltas.statement

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node._
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import deltas.expressions.ExpressionDelta
import deltas.javac.types.BooleanTypeDelta

object IfThenDelta extends DeltaWithGrammar with StatementInstance {

  def neww(condition: Node, thenBody: Any): Node = Shape.create(Condition -> condition, Then -> thenBody)

  object Shape extends NodeShape

  object Condition extends NodeField

  object Then extends NodeField

  override def dependencies: Set[Contract] = super.dependencies ++ Set(BlockDelta)

  def getCondition[T <: NodeLike](ifThen: T): T = {
    ifThen(Condition).asInstanceOf[T]
  }

  def getThenStatements[T <: NodeLike](ifThen: T): Seq[T] = {
    ifThen(Then).asInstanceOf[Seq[T]]
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

  override def description: String = "Enables using the if-then (no else) construct."

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, statement: NodePath, parentScope: Scope): Unit = {
    val bodyScope = builder.newScope(Some(parentScope), "thenScope")
    val body = IfThenDelta.getThenStatements(statement)
    BlockDelta.collectConstraints(compilation, builder, body, bodyScope)
    val condition = IfThenDelta.getCondition(statement)
    ExpressionDelta.constraints(compilation, builder, condition, BooleanTypeDelta.constraintType, parentScope)
  }

  override def shape: NodeShape = Shape
}
