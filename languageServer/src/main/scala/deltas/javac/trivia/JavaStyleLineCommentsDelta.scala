package deltas.javac.trivia

import core.bigrammar.grammars.{Colorize, RegexGrammar}
import core.bigrammar.{BiGrammar, TokenTypes}
import core.deltas.grammars.{LanguageGrammars, TriviaGrammar}
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.Language

object JavaStyleLineCommentsDelta extends DeltaWithGrammar {

  override def description: String = "Adds Java-style line comments to the language"

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    grammars.find(TriviaGrammar).addAlternative(commentGrammar)
  }

  val commentGrammar: BiGrammar = {
    val comment = RegexGrammar("""//[^(\n)]*\n""".r)
    Colorize(comment, TokenTypes.COMMENT_EOL)
  }

  override def dependencies: Set[Contract] = Set.empty
}
