package deltas.javac.trivia

import core.bigrammar._
import core.bigrammar.grammars._
import core.deltas.grammars.{LanguageGrammars, TriviaGrammar}
import core.deltas.{DeltaWithGrammar, Language}

object JavaStyleCommentsDelta extends DeltaWithGrammar {

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    grammars.find(TriviaGrammar).addOption(getCommentGrammar)
  }

  def getCommentGrammar: BiGrammar = {
    new Sequence(new RegexGrammar("""(?s)/\*.*?\*/""".r), space).ignoreRight
  }

  override def description: String = "Adds Java-style comments to the language"

}
