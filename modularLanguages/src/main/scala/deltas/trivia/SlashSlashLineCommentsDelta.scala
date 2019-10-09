package deltas.trivia

import core.bigrammar.BiGrammar
import core.bigrammar.grammars.{Colorize, RegexGrammar}
import core.deltas.grammars.{LanguageGrammars, TriviaGrammar}
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.Language

object SlashSlashLineCommentsDelta extends DeltaWithGrammar {

  override def description: String = "Adds // line comments to the language"

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    grammars.find(TriviaGrammar).addAlternative(commentGrammar)
  }

  val commentGrammar: BiGrammar = {
    val comment = RegexGrammar("""//[^\n]*\n""".r, "line comment")
    Colorize(comment, "comment.line.double-slash")
  }

  override def dependencies: Set[Contract] = Set.empty
}
