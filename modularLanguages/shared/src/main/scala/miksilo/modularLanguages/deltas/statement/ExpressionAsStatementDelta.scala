package miksilo.modularLanguages.deltas.javac.statements

import miksilo.modularLanguages.core.deltas.DeltaWithGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.modularLanguages.core.node._
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.modularLanguages.deltas.expression.ExpressionDelta
import miksilo.modularLanguages.deltas.statement.{StatementDelta, StatementInstance}

object ExpressionAsStatementDelta extends StatementInstance
  with DeltaWithGrammar {

  object Shape extends NodeShape {
    override def toString: String = "ExpressionAsStatement"
  }

  object Expression extends NodeField {
    override def toString: String = "Expression"
  }

  def create(expression: Node): Node = new Node(Shape, Expression -> expression)

  override val shape = Shape

  def getExpression[T <: NodeLike](statement: T): T = {
    statement(Expression).asInstanceOf[T]
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val expressionGrammar = find(ExpressionDelta.FirstPrecedenceGrammar)
    val statementGrammar = find(StatementDelta.Grammar)
    val expressionAsStatement = expressionGrammar.as(Expression) ~< ";" asNode Shape
    statementGrammar.addAlternative(expressionAsStatement)
  }

  override def description: String = "Enables using an expression as a statement."

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, statement: NodePath, parentScope: Scope): Unit = {
    ExpressionDelta.getType(compilation, builder, getExpression(statement), parentScope)
  }
}
