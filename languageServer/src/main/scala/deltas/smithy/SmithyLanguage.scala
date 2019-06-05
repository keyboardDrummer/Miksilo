package deltas.smithy

import core.deltas.{Delta, LanguageFromDeltas, ParseUsingTextualGrammar}
import core.language.Language

object SmithyLanguage {
  val deltas: Seq[Delta] = Seq.empty

  val language: Language = LanguageFromDeltas(Seq(ParseUsingTextualGrammar) ++ deltas)
}
