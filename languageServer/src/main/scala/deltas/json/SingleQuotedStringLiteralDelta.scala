package deltas.json

import core.bigrammar.grammars.RegexGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.Language
import deltas.expression.ExpressionDelta

object SingleQuotedStringLiteralDelta extends DeltaWithGrammar {

  override def description: String = "Adds the single quoted string literals"

  override def dependencies: Set[Contract] = Set(ExpressionDelta)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._

    val grammar = RegexGrammar("""'[^']*""".r).
      map[String, String](r => r.substring(1, r.length), s => "'" + s)
      .as(StringLiteralDelta.Value) ~< keyword("'")
    find(StringLiteralDelta.Shape).addAlternative(grammar.asNode(StringLiteralDelta.Shape))
  }
}
