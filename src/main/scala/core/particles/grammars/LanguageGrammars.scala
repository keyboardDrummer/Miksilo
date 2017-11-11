package core.particles.grammars

import core.bigrammar._
import core.particles.GrammarWithTrivia
import core.particles.node.{GrammarKey, Key}

case class KeyGrammar(key: Key) extends GrammarKey
{
  override lazy val toString = key.toString
}

object TriviaGrammar extends GrammarKey
object BodyGrammar extends GrammarKey
object ProgramGrammar extends GrammarKey
class LanguageGrammars extends GrammarCatalogue {

  val trivia: Labelled = create(TriviaGrammar, ParseWhiteSpace)
  val bodyGrammar = create(BodyGrammar, BiFailure())
  create(ProgramGrammar, new WithTrivia(new IgnoreRight(new Sequence(bodyGrammar, trivia)), trivia))

  def root: Labelled = find(ProgramGrammar)

  implicit def stringToAstGrammar(value: String) = new GrammarWithTrivia(Keyword(value))(this)
  implicit def grammarToAstGrammar(value: BiGrammar) = new GrammarWithTrivia(value)(this)


}

case class GrammarNotFoundException(key: Any, inner: Exception) extends RuntimeException(inner)
{
  override def toString = s"Could not find grammar $key."
}
