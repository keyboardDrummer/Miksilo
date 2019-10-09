package deltas.json

import core.bigrammar.grammars.{Colorize, Delimiter}
import core.deltas.grammars.LanguageGrammars
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.Language
import core.language.node.GrammarKey
import deltas.expression.{ExpressionDelta, StringLiteralDelta}
import deltas.json.JsonStringLiteralDelta.Value

object SingleQuotedStringLiteralDelta extends DeltaWithGrammar {

  override def description: String = "Adds the single quoted string literals"

  override def dependencies: Set[Contract] = Set(ExpressionDelta)

  object Grammar extends GrammarKey
  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    val inner = {
      val withoutColor = JsonStringLiteralDelta.dropPrefix(grammars,
        grammars.regexGrammar("""'[^']*""".r, "single quote string literal"), JsonStringLiteralDelta.Value, "'") ~< Delimiter("'")
      Colorize(withoutColor, "string.quoted.single")
    }
    import grammars._
    find(StringLiteralDelta.Shape).addAlternative(create(Grammar, inner.asNode(StringLiteralDelta.Shape)))
  }
}
