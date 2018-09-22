package deltas.json

import core.deltas.{Delta, ParseUsingTextualGrammar}
import deltas.expression.IntLiteralDelta
import deltas.expressions.ExpressionDelta
import deltas.javac.expressions.ExpressionLanguageDelta
import deltas.javac.expressions.literals.BooleanLiteralDelta

object JsonLanguage {
  val deltas: Seq[Delta] = Seq[Delta](ExpressionLanguageDelta, BooleanLiteralDelta, JsonObjectLiteralDelta,
    JsonArrayLiteralDelta, JsonStringLiteralDelta, IntLiteralDelta, ExpressionDelta, ParseUsingTextualGrammar)
  val language = Delta.buildLanguage(deltas)
}
