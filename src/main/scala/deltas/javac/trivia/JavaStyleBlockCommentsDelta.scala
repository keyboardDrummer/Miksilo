package deltas.javac.trivia

import core.bigrammar._
import core.bigrammar.grammars._
import core.deltas.grammars.{LanguageGrammars, TriviaGrammar}
import core.deltas.DeltaWithGrammar
import core.language.Language
import org.fife.ui.rsyntaxtextarea.TokenTypes

object JavaStyleBlockCommentsDelta extends DeltaWithGrammar {

  override def description: String = "Adds Java-style block comments to the language"

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    grammars.find(TriviaGrammar).addAlternative(commentGrammar)
  }

  val commentGrammar: BiGrammar = {
    import BasicSequenceCombinators._

    val comment = RegexGrammar("""(?s)/\*[^(\*/)]*\*/""".r)
    val coloredComment = Colorize(comment, TokenTypes.COMMENT_MULTILINE)
    (coloredComment ~ printSpace).ignoreRight
  }
}
