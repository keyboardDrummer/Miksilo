package core.deltas.grammars

import core.bigrammar._
import core.bigrammar.grammars._
import core.deltas.{GrammarForAst, NodeGrammarWriter}
import core.language.node.{GrammarKey, Key, NodeShape}

case class KeyGrammar(key: Key) extends GrammarKey
{
  override lazy val toString = key.toString
}

object TriviasGrammar extends GrammarKey
object TriviaGrammar extends GrammarKey
object BodyGrammar extends GrammarKey
object ProgramGrammar extends GrammarKey

class LanguageGrammars extends GrammarCatalogue with NodeGrammarWriter {

  val trivia: Labelled = create(TriviasGrammar, new ManyVertical(create(TriviaGrammar, ParseWhiteSpace)))
  val bodyGrammar = create(BodyGrammar, BiFailure())
  create(ProgramGrammar, new WithTrivia(leftRight(bodyGrammar, trivia, BiSequence.ignoreRight), trivia)) //TODO Move this, bodyGrammar and trivia to a separate Delta.

  def root: Labelled = find(ProgramGrammar)

  implicit def stringToAstGrammar(value: String): BiGrammarExtension =
    new BiGrammarExtension(stringToGrammar(value), this)

  implicit def grammarToAstGrammar(value: BiGrammar): BiGrammarExtension = new BiGrammarExtension(value, this)

  def addTriviaIfUseful(grammar: BiGrammar, horizontal: Boolean) =
    if (grammar.containsParser())
      new WithTrivia(grammar, trivia, horizontal)
    else
      grammar

  class BiGrammarExtension(val grammar: BiGrammar, grammars: LanguageGrammars) extends NodeGrammarWriter
    with BiGrammarSequenceCombinatorsExtension
  {

    def asLabelledNode(key: NodeShape): Labelled = grammars.create(key, new GrammarForAst(grammar).asNode(key))

    def manyVertical = new ManyVertical(addTriviaIfUseful(grammar, horizontal = false))

    override def sequence(other: BiGrammar, bijective: SequenceBijective, horizontal: Boolean): BiGrammar =
      new BiSequence(grammar, addTriviaIfUseful(other, horizontal), bijective, horizontal)

    def many = new ManyHorizontal(addTriviaIfUseful(grammar, horizontal = true))

    override implicit def addSequenceMethods(grammar: BiGrammar): BiGrammarExtension = new BiGrammarExtension(grammar, grammars)
  }
}


