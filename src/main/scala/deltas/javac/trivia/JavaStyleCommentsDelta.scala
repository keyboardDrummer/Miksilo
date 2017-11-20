package deltas.javac.trivia

import core.bigrammar._
import core.bigrammar.grammars._
import core.deltas.grammars.{LanguageGrammars, TriviaGrammar}
import core.deltas.{DeltaWithGrammar, Language}

object JavaStyleCommentsDelta extends DeltaWithGrammar {

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    /*Because whitespace and comments are both stored as strings,
      we try comments first and verify using parsing that something is a comment.
     */
    grammars.find(TriviaGrammar).inner = getCommentGrammar | grammars.find(TriviaGrammar).inner
  }

  def getCommentGrammar: BiGrammar = {
    new Sequence(RegexGrammar("""(?s)/\*.*?\*/""".r, verifyWhenPrinting = true), space).ignoreRight
  }

  override def description: String = "Adds Java-style comments to the language"

}
