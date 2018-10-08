package deltas.javac.trivia

import core.bigrammar._
import core.bigrammar.grammars._
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

object JavaStyleBlockCommentsDelta extends DeltaWithGrammar {

  override def description: String = "Adds Java-style block comments to the language"

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    grammars.find(TriviaGrammar).addAlternative(commentGrammar)
  }

  val commentGrammar: BiGrammar = {
    import BasicSequenceCombinators._

    val comment = RegexGrammar("""/\*+[^*]*\*+(?:[^/*][^*]*\*+)*/""".r)
    val coloredComment = Colorize(comment, TokenTypes.COMMENT_MULTILINE)
    (coloredComment ~ printSpace).ignoreRight
  }

  override def dependencies: Set[Contract] = Set.empty
}
