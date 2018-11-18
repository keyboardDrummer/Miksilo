package core.bigrammar

import core.bigrammar.grammars._

trait WhitespaceTriviaSequenceCombinators extends BiGrammarWriter {

  def addTriviaIfUseful(grammar: BiGrammar, horizontal: Boolean = true): BiGrammar =
    if (grammar.containsParser()) WithTrivia.withTrivia(grammar, new ManyHorizontal(ParseWhiteSpace), horizontal) else grammar

  implicit def stringAsGrammar(value: String): BiGrammarExtension = new BiGrammarExtension(value)
  implicit class BiGrammarExtension(val grammar: BiGrammar) extends BiGrammarSequenceCombinatorsExtension {
    def manyVertical = new ManyVertical(addTriviaIfUseful(grammar, horizontal = false))

    def leftRight(other: BiGrammar, bijective: SequenceBijective) =
      new LeftRight(grammar, addTriviaIfUseful(other), bijective)

    def many = new ManyHorizontal(addTriviaIfUseful(grammar))

    def topBottom(bottom: BiGrammar, bijective: SequenceBijective) =
      new TopBottom(grammar, addTriviaIfUseful(bottom, horizontal = false), bijective)

    override implicit def addSequenceMethods(grammar: BiGrammar): BiGrammarSequenceCombinatorsExtension = new BiGrammarExtension(grammar)
  }
}
