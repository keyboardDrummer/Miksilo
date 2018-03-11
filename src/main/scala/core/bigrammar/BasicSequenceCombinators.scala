package core.bigrammar

import core.bigrammar.grammars._

object BasicSequenceCombinators {
  implicit def stringToAstGrammar(value: String): BiGrammarExtension =
    new BiGrammarExtension(BiGrammarWriter.stringToGrammar(value))
  implicit def grammarToAstGrammar(value: BiGrammar): BiGrammarExtension = new BiGrammarExtension(value)

  class BiGrammarExtension(val grammar: BiGrammar) extends BiGrammarSequenceCombinatorsExtension {

    override def %(bottom: BiGrammar): Sequence = new TopBottom(grammar, bottom)

    override def ~(other: BiGrammar): Sequence = new LeftRight(grammar, other)

    override def many: ManyHorizontal = new ManyHorizontal(grammar)

    override def manyVertical: ManyVertical = new ManyVertical(grammar)

    override implicit def addSequenceMethods(grammar: BiGrammar): BiGrammarSequenceCombinatorsExtension = new BiGrammarExtension(grammar)
  }
}
