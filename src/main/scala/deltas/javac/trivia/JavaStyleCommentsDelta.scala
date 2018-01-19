package deltas.javac.trivia

import core.bigrammar._
import core.bigrammar.grammars._
import core.deltas.grammars.{LanguageGrammars, TriviaGrammar}
import core.deltas.{DeltaWithGrammar, Language}
import org.fife.ui.rsyntaxtextarea.TokenTypes

object JavaStyleCommentsDelta extends DeltaWithGrammar {

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    grammars.find(TriviaGrammar).addOption(getCommentGrammar)
  }

  def getCommentGrammar: BiGrammar = {
    val comment = TokenColor(RegexGrammar("""(?s)/\*[^(\*/)]*\*/""".r), TokenTypes.COMMENT_MULTILINE)
    new LeftRight(comment, space).ignoreRight
  }

  override def description: String = "Adds Java-style comments to the language"

}
