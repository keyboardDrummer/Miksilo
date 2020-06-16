package miksilo.modularLanguages.deltas.yaml

import miksilo.languageServer.core.language.Language
import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.bigrammar.grammars.{Colorize, RegexGrammar}
import miksilo.modularLanguages.core.deltas.grammars.{LanguageGrammars, TriviaGrammar}
import miksilo.modularLanguages.core.deltas.{Contract, DeltaWithGrammar}

object HashLineCommentsDelta extends DeltaWithGrammar {

  override def description: String = "Adds # line comments to the language"

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    grammars.find(TriviaGrammar).addAlternative(commentGrammar)
  }

  val commentGrammar: BiGrammar = {
    val comment = RegexGrammar("""#[^\r\n]*\r?\n""".r, "line comment")
    Colorize(comment, "comment.line.hash")
  }

  override def dependencies: Set[Contract] = Set.empty
}
