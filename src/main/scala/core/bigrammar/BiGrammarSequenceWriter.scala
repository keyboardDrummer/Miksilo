package core.bigrammar

import core.bigrammar.grammars._

trait BiGrammarSequenceWriter extends BiGrammarWriter {

  def addTriviaIfUseful(grammar: BiGrammar, horizontal: Boolean = true) =
    if (grammar.containsParser()) new WithTrivia(grammar, new ManyHorizontal(ParseWhiteSpace), horizontal) else grammar

  implicit def stringAsGrammar(value: String) = new GrammarWithSequence(value)
  implicit class GrammarWithSequence(val grammar: BiGrammar) extends BiGrammarSequenceMethodsExtension {
    def manyVertical = new ManyVertical(addTriviaIfUseful(grammar, horizontal = false))

    def ~(other: BiGrammar) = new LeftRight(grammar, addTriviaIfUseful(other))

    def many = new ManyHorizontal(addTriviaIfUseful(grammar))

    def %(bottom: BiGrammar) = new TopBottom(grammar, addTriviaIfUseful(bottom, horizontal = false))

    override implicit def addSequenceMethods(grammar: BiGrammar): BiGrammarSequenceMethodsExtension = new GrammarWithSequence(grammar)
  }
}
