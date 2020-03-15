package miksilo.modularLanguages.deltas.trivia

import miksilo.modularLanguages.core.bigrammar.grammars.{Colorize, RegexGrammar}
import miksilo.modularLanguages.core.bigrammar.{BiGrammar, DefaultBiGrammarWriter}
import miksilo.modularLanguages.core.deltas.grammars.{LanguageGrammars, TriviaGrammar}
import miksilo.modularLanguages.core.deltas.{Contract, DeltaWithGrammar}
import miksilo.languageServer.core.language.Language

object SlashStarBlockCommentsDelta extends DeltaWithGrammar with DefaultBiGrammarWriter {

  override def description: String = "Adds /*..*/ block comments to the language"

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    grammars.find(TriviaGrammar).addAlternative(commentGrammar)
  }

  val commentGrammar: BiGrammar = {
    val comment = RegexGrammar("""/\*+[^*]*\*+(?:[^/*][^*]*\*+)*/""".r, "block comment")
    Colorize(comment, "comment.block") ~< printSpace
  }

  override def dependencies: Set[Contract] = Set.empty
}
