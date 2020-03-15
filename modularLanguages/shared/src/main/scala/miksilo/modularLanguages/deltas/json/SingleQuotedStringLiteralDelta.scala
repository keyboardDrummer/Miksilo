package miksilo.modularLanguages.deltas.json

import miksilo.modularLanguages.core.bigrammar.grammars.{Colorize, Delimiter}
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.{Contract, DeltaWithGrammar}
import miksilo.languageServer.core.language.Language
import miksilo.modularLanguages.core.node.GrammarKey
import miksilo.modularLanguages.deltas.expression.{ExpressionDelta, StringLiteralDelta}
import miksilo.modularLanguages.deltas.json.JsonStringLiteralDelta.Value

object SingleQuotedStringLiteralDelta extends DeltaWithGrammar {

  override def description: String = "Adds the single quoted string literals"

  override def dependencies: Set[Contract] = Set(ExpressionDelta)

  object Grammar extends GrammarKey
  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    val inner = {
      import miksilo.modularLanguages.core.bigrammar.DefaultBiGrammarWriter._
      val withoutColor = JsonStringLiteralDelta.dropPrefix(grammars,
        grammars.regexGrammar("""'[^']*""".r, "single quote string literal"), JsonStringLiteralDelta.Value, "'") ~< Delimiter("'")
      Colorize(withoutColor, "string.quoted.single")
    }
    import grammars._
    find(StringLiteralDelta.Shape).addAlternative(create(Grammar, inner.asNode(StringLiteralDelta.Shape)))
  }
}
