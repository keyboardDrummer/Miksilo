package deltas.json

import core.deltas.{Delta, ParseUsingTextualGrammar}
import deltas.expression.{IntLiteralDelta, StringLiteralDelta}
import deltas.javac.expressions.literals.BooleanLiteralDelta
import deltas.javac.expressions.{ExpressionLanguageDelta, ExpressionSkeleton}

object JsonLanguage {
  val deltas: Seq[Delta] = Seq[Delta](ExpressionLanguageDelta, BooleanLiteralDelta, JsonObjectLiteralDelta, JsonArrayLiteralDelta, StringLiteralDelta, IntLiteralDelta, ExpressionSkeleton, ParseUsingTextualGrammar)
  val language = Delta.buildLanguage(deltas)
}
