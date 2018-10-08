package deltas.json

import core.deltas.{Delta, LanguageFromDeltas}
import deltas.expression.IntLiteralDelta
import deltas.expressions.ExpressionDelta
import deltas.javac.expressions.ExpressionLanguageDelta
import deltas.javac.expressions.literals.BooleanLiteralDelta

object JsonLanguage {
  val deltas: Seq[Delta] = Seq[Delta](ExpressionLanguageDelta, BooleanLiteralDelta, JsonObjectLiteralDelta,
    JsonArrayLiteralDelta, JsonStringLiteralDelta, IntLiteralDelta, ExpressionDelta)
  val language = LanguageFromDeltas(deltas)
}
