package deltas.json

import core.deltas.{Delta, LanguageFromDeltas}
import deltas.expression._
import deltas.javac.expressions.literals.BooleanLiteralDelta

object JsonLanguage {
  val deltas: Seq[Delta] = Seq[Delta](ExpressionLanguageDelta, DefaultExpressionDelta, BooleanLiteralDelta, JsonObjectLiteralDelta,
    ArrayLiteralDelta, JsonStringLiteralDelta, IntLiteralDelta, ExpressionDelta)
  val language = LanguageFromDeltas(deltas)
}
