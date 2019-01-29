package deltas.javac

import core.deltas.Delta
import deltas.expression.additive.{AdditionDelta, AdditivePrecedenceDelta, SubtractionDelta}
import deltas.expression.logical.LogicalNotDelta
import deltas.expression.relational.{AddRelationalPrecedenceDelta, EqualsComparisonDelta, GreaterThanDelta, LessThanDelta}
import deltas.expression.{ExpressionDelta, IntLiteralDelta, ParenthesisInExpressionDelta, TernaryDelta}
import deltas.javac.JavaToByteCodeLanguage.noVariableSyntaxSugarStatements
import deltas.javac.expressions.equality.AddEqualityPrecedence
import deltas.javac.expressions.literals.{BooleanLiteralDelta, LongLiteralDelta, NullDelta}
import deltas.javac.statements.ExpressionAsStatementDelta
import deltas.statement._

object JavaLanguage {

  def deltas = javaSimpleExpression

  def simpleBlock: Seq[Delta] = noVariableSyntaxSugarStatements ++
    Seq(GotoStatementDelta, LabelStatementDelta,
      IfThenElseDelta, IfThenDelta,
      BlockDelta, ExpressionAsStatementDelta, StatementDelta) ++ javaSimpleExpression

  def javaSimpleExpression: Seq[Delta] = Seq(TernaryDelta, EqualsComparisonDelta,
    AddEqualityPrecedence, LessThanDelta, GreaterThanDelta,
    AddRelationalPrecedenceDelta, AdditionDelta,
    SubtractionDelta, AdditivePrecedenceDelta,
    BooleanLiteralDelta, LongLiteralDelta, IntLiteralDelta,
    NullDelta, LogicalNotDelta, ParenthesisInExpressionDelta, ExpressionDelta)
}
