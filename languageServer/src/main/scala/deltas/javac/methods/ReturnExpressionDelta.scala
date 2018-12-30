package deltas.javac.methods

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node._
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import deltas.expressions.ExpressionDelta
import deltas.statement.{StatementDelta, StatementInstance}

object ReturnExpressionDelta extends StatementInstance
  with DeltaWithGrammar {

  override def description: String = "Allows returning a value using an expression."

  override def dependencies: Set[Contract] = Set(StatementDelta, ExpressionDelta)

  def getReturnValue[T <: NodeLike](_return: T) = _return(ReturnValue).asInstanceOf[T]

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val expression = find(ExpressionDelta.FirstPrecedenceGrammar)
    val statement = find(StatementDelta.Grammar)

    val returnExpression = "return" ~~> expression.as(ReturnValue) ~< ";" asNode Shape
    statement.addAlternative(returnExpression)
  }

  def neww(value: Node): Node = new Node(Shape, ReturnValue -> value)

  object Shape extends NodeShape

  object ReturnValue extends NodeField

  override val shape = Shape

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, statement: NodePath, parentScope: Scope): Unit = {
    ExpressionDelta.getType(compilation, builder, getReturnValue(statement), parentScope)
  }
}
