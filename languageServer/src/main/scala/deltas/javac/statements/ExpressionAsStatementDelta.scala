package deltas.javac.statements

import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node._
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import deltas.bytecode.coreInstructions.{Pop2Delta, PopDelta}
import deltas.bytecode.types.TypeSkeleton
import deltas.expressions.ExpressionDelta
import deltas.javac.expressions.ToByteCodeSkeleton
import deltas.statement.{StatementDelta, StatementInstance}

object ExpressionAsStatementDelta extends ByteCodeStatementInstance with StatementInstance {

  object Shape extends NodeShape

  object Expression extends NodeField

  def create(expression: Node): Node = new Node(Shape, Expression -> expression)

  override val shape = Shape

  override def toByteCode(statement: NodePath, compilation: Compilation): Seq[Node] = {
    val expression = getExpression(statement)
    val _type = ExpressionDelta.getType(compilation)(expression)
    val extra = TypeSkeleton.getTypeSize(_type, compilation) match {
      case 0 => Seq.empty
      case 1 => Seq(PopDelta.pop)
      case 2 => Seq(Pop2Delta.pop2)
    }
    ToByteCodeSkeleton.getToInstructions(compilation)(expression) ++ extra
  }

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
