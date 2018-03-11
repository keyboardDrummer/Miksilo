package core.bigrammar

import core.bigrammar.grammars._

trait WhitespaceTriviaSequenceCombinators extends BiGrammarWriter {

  def addTriviaIfUseful(grammar: BiGrammar, horizontal: Boolean = true) =
    if (grammar.containsParser()) new WithTrivia(grammar, new ManyHorizontal(ParseWhiteSpace), horizontal) else grammar

  implicit def stringAsGrammar(value: String): BiGrammarExtension = new BiGrammarExtension(value)
  implicit class BiGrammarExtension(val grammar: BiGrammar) extends BiGrammarSequenceCombinatorsExtension {
    def manyVertical = new ManyVertical(addTriviaIfUseful(grammar, horizontal = false))

    def ~(other: BiGrammar) = new LeftRight(grammar, addTriviaIfUseful(other))

    def many = new ManyHorizontal(addTriviaIfUseful(grammar))

    def %(bottom: BiGrammar) = new TopBottom(grammar, addTriviaIfUseful(bottom, horizontal = false))

    override implicit def addSequenceMethods(grammar: BiGrammar): BiGrammarSequenceCombinatorsExtension = new BiGrammarExtension(grammar)
  }
}
