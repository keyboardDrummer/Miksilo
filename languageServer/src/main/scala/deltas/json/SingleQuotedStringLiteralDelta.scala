package deltas.json

import core.bigrammar.grammars.RegexGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.Language
import core.language.node.SourceRange
import deltas.expression.ExpressionDelta
import deltas.json.StringLiteralDelta.Value
import langserver.types.Position

object SingleQuotedStringLiteralDelta extends DeltaWithGrammar {

  override def description: String = "Adds the single quoted string literals"

  override def dependencies: Set[Contract] = Set(ExpressionDelta)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._

    val grammar = RegexGrammar("""'[^']*""".r).
      map[String, String](r => r.substring(1, r.length), s => "\"" + s).as(Value,
      p => SourceRange(Position(p.start.line, p.start.character + 1), p.end)) ~< keyword("'")
    find(ExpressionDelta.FirstPrecedenceGrammar).addAlternative(grammar.asNode(StringLiteralDelta.Shape))
  }
}
