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

object BlockScalarDelta extends DeltaWithGrammar {
  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    val _grammars = grammars
    import grammars._

    val nbChar = RegexGrammar("""[^\r\n]+""".r, "non break character")
    val chompingIndicator: BiGrammar = "-" | "+" | value("")
    val newLine = RegexGrammar("""\r?\n""".r, "newLine", penaltyOption = Some(History.failPenalty), allowDrop = false)
    val lineSeparator = new BiSequence(newLine,
      _grammars.trivia, BiSequence.ignoreLeft, true)

    val lines: BiGrammar = {
      val line = CheckIndentationGrammar.greaterThanOrEqualTo(nbChar)
      CheckIndentationGrammar.greaterThan(new WithIndentationGrammar(line.someSeparated(lineSeparator)))
    }

    val literal = new WithIndentationGrammar(("|" | ">" ) ~ chompingIndicator ~ lineSeparator) ~> lines

    find(IndentationSensitiveExpression).addAlternative(literal.
      as(StringLiteralDelta.Value).asLabelledNode(StringLiteralDelta.Shape))
  }

  override def description = "Introduce literal and folded scalar styles"

  override def dependencies = Set(YamlCoreDelta)
}
