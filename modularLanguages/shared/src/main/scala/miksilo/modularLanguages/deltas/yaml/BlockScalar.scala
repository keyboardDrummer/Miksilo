package miksilo.modularLanguages.deltas.yaml

import miksilo.editorParser.parsers.editorParsers.History
import miksilo.languageServer.core.language.Language
import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.bigrammar.grammars.{BiSequence, Delimiter, RegexGrammar}
import miksilo.modularLanguages.core.deltas.DeltaWithGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.deltas.expression.StringLiteralDelta
import miksilo.modularLanguages.deltas.json.JsonStringLiteralDelta
import miksilo.modularLanguages.deltas.yaml.YamlCoreDelta.IndentationSensitiveExpression

object BlockScalar extends DeltaWithGrammar {
  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    val _grammars = grammars
    import grammars._

    val nbChar = RegexGrammar("""[^\n]+""".r, "non break character")
    val chompingIndicator: BiGrammar = "-" | "+" | value("")
    val lineSeparator = new BiSequence(Delimiter("\n", penalty = History.failPenalty, allowDrop = false), _grammars.trivia, BiSequence.ignoreLeft, true)

    val lines: BiGrammar = {
      val line = CheckIndentationGrammar.greaterThanOrEqualTo(nbChar)
      CheckIndentationGrammar.greaterThan(new Withy
        IndentationGrammar(line.someSeparated(lineSeparator)))
    }

    val literal = new WithIndentationGrammar(("|" | ">" ) ~ chompingIndicator ~ lineSeparator) ~> lines

    find(IndentationSensitiveExpression).addAlternative(literal.
      as(JsonStringLiteralDelta.Value).asLabelledNode(StringLiteralDelta.Shape))
  }

  override def description = "Introduce literal and folded scalar styles"

  override def dependencies = Set(YamlCoreDelta)
}
