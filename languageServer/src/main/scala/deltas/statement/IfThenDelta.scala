package deltas.statement

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node._
import deltas.javac.expressions.ExpressionSkeleton
import deltas.javac.statements.BlockDelta

object IfThenDelta extends DeltaWithGrammar {

  def neww(condition: Node, thenBody: Seq[Node]): Node = Shape.create(Condition -> condition, Then -> thenBody)

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
    val expressionGrammar = find(ExpressionSkeleton.ExpressionGrammar)
    val bodyGrammar = find(BlockDelta.BlockOrStatementGrammar)
    val ifThenGrammar = create(Shape, ("if" ~> ("(" ~> expressionGrammar.as(Condition) ~< ")") % bodyGrammar.as(Then)).
      asNode(Shape))
    statementGrammar.addAlternative(ifThenGrammar)
  }

  override def description: String = "Enables using the if-then (no else) construct."
}
