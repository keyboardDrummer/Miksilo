package deltas.json

import core.bigrammar.grammars.RegexGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.Language
import core.language.node.GrammarKey
import deltas.expression.ExpressionDelta

object SingleQuotedStringLiteralDelta extends DeltaWithGrammar {

  override def description: String = "Adds the single quoted string literals"

  override def dependencies: Set[Contract] = Set(ExpressionDelta)

  object Grammar extends GrammarKey
  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._

    val grammar = keyword("'") ~> RegexGrammar("""[^']*""".r).as(StringLiteralDelta.Value) ~<
      keyword("'") asNode StringLiteralDelta.Shape
    find(StringLiteralDelta.Shape).addAlternative(create(Grammar, grammar))
  }
}
