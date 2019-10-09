package deltas.json

import core.bigrammar.textMate.BiGrammarToTextMate
import core.deltas.{Delta, LanguageFromDeltas, ParseUsingTextualGrammar}
import core.language.{Language, Phase}
import deltas.expression._
import deltas.javac.expressions.literals.BooleanLiteralDelta

object JsonLanguage {
  val deltas: Seq[Delta] = Seq[Delta](ExpressionLanguageDelta, BooleanLiteralDelta, JsonObjectLiteralDelta,
    ArrayLiteralDelta, SingleQuotedStringLiteralDelta, JsonStringLiteralDelta, IntLiteralDelta, ExpressionDelta)
  val language = LanguageFromDeltas(Seq(ParseUsingTextualGrammar()) ++ deltas)
}

object PrintJson extends Delta  {

  override def inject(language: Language): Unit = {
    language.compilerPhases = List(Phase(this, compilation => {
      try
      {
        compilation.output = BiGrammarToTextMate.printJson(compilation.program)
      } catch {
        case _: Throwable =>
      }
    }))
  }

  override def description = "Prints the current program as Json to the output"

  override def dependencies = Set.empty
}