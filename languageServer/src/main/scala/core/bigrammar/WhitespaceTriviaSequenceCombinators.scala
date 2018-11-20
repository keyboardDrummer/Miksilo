package core.bigrammar

import core.bigrammar.grammars._
import core.deltas.grammars.TriviasGrammar

trait WhitespaceTriviaSequenceCombinators extends BiGrammarWriter {

  def addTriviaIfUseful(grammar: BiGrammar, horizontal: Boolean = true): BiGrammar =
    if (grammar.containsParser()) WithTrivia.withTrivia(grammar, new Labelled(TriviasGrammar, new ManyHorizontal(ParseWhiteSpace)), horizontal) else grammar

  implicit def stringAsGrammar(value: String): BiGrammarExtension = new BiGrammarExtension(value)
  implicit class BiGrammarExtension(val grammar: BiGrammar) extends BiGrammarSequenceCombinatorsExtension {
    def manyVertical = new ManyVertical(addTriviaIfUseful(grammar, horizontal = false))


    override def sequence(other: BiGrammar, bijective: SequenceBijective, horizontal: Boolean): BiGrammar =
      new BiSequence(grammar, addTriviaIfUseful(other), bijective, horizontal)

    def many = new ManyHorizontal(addTriviaIfUseful(grammar))

    override implicit def addSequenceMethods(grammar: BiGrammar): BiGrammarSequenceCombinatorsExtension = new BiGrammarExtension(grammar)
  }
}
