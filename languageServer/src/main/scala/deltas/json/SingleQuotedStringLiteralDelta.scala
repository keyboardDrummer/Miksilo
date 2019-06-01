package deltas.json

import core.bigrammar.grammars.RegexGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.Language
import deltas.expression.ExpressionDelta
import deltas.json.StringLiteralDelta.Value

object SingleQuotedStringLiteralDelta extends DeltaWithGrammar {

  override def description: String = "Adds the single quoted string literals"

  override def dependencies: Set[Contract] = Set(ExpressionDelta)

  override def transformGrammars(_grammars: LanguageGrammars, state: Language): Unit = {
    val grammars = _grammars
    import grammars._

    val grammar = StringLiteralDelta.dropPrefix(grammars,
      grammars.regexGrammar("""'[^']*""".r, "single quote string literal"), Value, "'") ~< keywordGrammar("'")
    find(ExpressionDelta.FirstPrecedenceGrammar).addAlternative(grammar.asNode(StringLiteralDelta.Shape))
  }
}
