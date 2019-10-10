package core.bigrammar

import core.bigrammar.grammars.{BiSequence, ManyHorizontal, ManyVertical, SequenceBijective}

object DefaultBiGrammarWriter extends DefaultBiGrammarWriter

trait DefaultBiGrammarWriter extends BiGrammarWriter {

  implicit def stringToAstGrammar(value: String): BiGrammarExtension =
    new BiGrammarExtension(BiGrammarWriter.stringToGrammar(value))

  implicit def grammarToAstGrammar(value: BiGrammar): BiGrammarExtension = new BiGrammarExtension(value)

  class BiGrammarExtension(val grammar: BiGrammar) extends BiGrammarSequenceCombinatorsExtension {

    override def many: ManyHorizontal = new ManyHorizontal(grammar)

    override def manyVertical: ManyVertical = new ManyVertical(grammar)

    override implicit def addSequenceMethods(grammar: BiGrammar): BiGrammarSequenceCombinatorsExtension = new BiGrammarExtension(grammar)

    override def sequence(other: BiGrammar, bijective: SequenceBijective, horizontal: Boolean): BiGrammar =
      new BiSequence(grammar, other, bijective, horizontal)
  }
}
