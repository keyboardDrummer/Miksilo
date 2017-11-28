package core.deltas.grammars

import core.bigrammar._
import core.bigrammar.grammars._
import core.deltas.GrammarWithTrivia
import core.deltas.node.{GrammarKey, Key}

case class KeyGrammar(key: Key) extends GrammarKey
{
  override lazy val toString = key.toString
}

object TriviasGrammar extends GrammarKey
object TriviaGrammar extends GrammarKey
object BodyGrammar extends GrammarKey
object ProgramGrammar extends GrammarKey
class LanguageGrammars extends GrammarCatalogue {

  val trivia: Labelled = create(TriviasGrammar, new ManyVertical(create(TriviaGrammar, ParseWhiteSpace)))
  val bodyGrammar = create(BodyGrammar, BiFailure())
  create(ProgramGrammar, new WithTrivia(new IgnoreRight(new LeftRight(bodyGrammar, trivia)), trivia))

  def root: Labelled = find(ProgramGrammar)

  implicit def stringToAstGrammar(value: String) = new GrammarWithTrivia(Keyword(value), this)
  implicit def grammarToAstGrammar(value: BiGrammar) = new GrammarWithTrivia(value, this)
}
