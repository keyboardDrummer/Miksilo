package deltas.json

import core.bigrammar.grammars.{Colorize, Delimiter, RegexGrammar}
import core.deltas.grammars.LanguageGrammars
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.Language
import deltas.expression.ExpressionDelta

object SingleQuotedStringLiteralDelta extends DeltaWithGrammar {

  override def description: String = "Adds the single quoted string literals"

  override def dependencies: Set[Contract] = Set(ExpressionDelta)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {

    val inner = {
      import core.bigrammar.DefaultBiGrammarWriter._
      Colorize(Delimiter("'") ~> RegexGrammar("""[^']*""".r).as(StringLiteralDelta.Value) ~< Delimiter("'"), "string.quoted.single")
    }
    import grammars._
    find(StringLiteralDelta.Shape).addAlternative(inner.asNode(StringLiteralDelta.Shape))
  }
}
