package miksilo.modularLanguages.deltas.javac.methods

import miksilo.modularLanguages.core.deltas._
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.modularLanguages.core.node._
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.modularLanguages.deltas.expression.ExpressionDelta
import miksilo.modularLanguages.deltas.statement.{StatementDelta, StatementInstance}

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
