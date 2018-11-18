package deltas.javac.trivia

import core.bigrammar._
import core.bigrammar.grammars._
import core.deltas.grammars.{LanguageGrammars, TriviaGrammar}
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.Language

object JavaStyleBlockCommentsDelta extends DeltaWithGrammar with DefaultBiGrammarWriter {

  override def description: String = "Adds Java-style block comments to the language"

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    grammars.find(TriviaGrammar).addAlternative(commentGrammar)
  }

  val commentGrammar: BiGrammar = {

    val comment = RegexGrammar("""/\*+[^*]*\*+(?:[^/*][^*]*\*+)*/""".r)
    val coloredComment = Colorize(comment, TokenTypes.COMMENT_MULTILINE)
    new LeftRight(coloredComment, printSpace, Sequence.ignoreRight)
  }

  override def dependencies: Set[Contract] = Set.empty
}
