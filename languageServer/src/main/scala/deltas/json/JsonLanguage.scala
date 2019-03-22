package deltas.json

import core.deltas.{Delta, LanguageFromDeltas, ParseUsingTextualGrammar}
import deltas.expression._
import deltas.javac.expressions.literals.BooleanLiteralDelta

object JsonLanguage {
  val deltas: Seq[Delta] = Seq[Delta](ExpressionLanguageDelta, DefaultExpressionDelta, BooleanLiteralDelta, JsonObjectLiteralDelta,
    ArrayLiteralDelta, SingleQuoteStringLiteralDelta, DoubleQuoteStringLiteralDelta, IntLiteralDelta, ExpressionDelta)
  val language = LanguageFromDeltas(Seq(ParseUsingTextualGrammar) ++ deltas)
}
