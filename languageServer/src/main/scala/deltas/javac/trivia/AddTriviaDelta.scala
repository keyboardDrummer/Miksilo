package deltas.javac.trivia

import core.bigrammar.grammars._
import core.bigrammar.{BiGrammar, BiGrammarSequenceCombinatorsExtension, BiGrammarWriter}
import core.deltas.grammars.LanguageGrammars
import core.deltas.{Contract, Delta, GrammarForAst, NodeGrammarWriter}
import core.language.Language
import core.language.node.NodeShape

object AddTriviaDelta extends Delta {
  override def description: String = "Adds trivia to the language"

  implicit class AddTriviaCombinator(language: Language) {
    def triviaCombinators = new TriviaCombinators(language)
  }

  class TriviaCombinators(language: Language) {
    implicit def stringToAstGrammar(value: String): BiGrammarExtension =
      new BiGrammarExtension(BiGrammarWriter.stringToGrammar(value), language.grammars)
    implicit def grammarToAstGrammar(value: BiGrammar): BiGrammarExtension = new BiGrammarExtension(value, language.grammars)
  }

  class BiGrammarExtension(val grammar: BiGrammar, grammars: LanguageGrammars) extends NodeGrammarWriter
    with BiGrammarSequenceCombinatorsExtension
  {
    private def addTriviaIfUseful(grammar: BiGrammar, horizontal: Boolean = true) =
      if (grammar.containsParser()) new WithTrivia(grammar, grammars.trivia, horizontal) else grammar

    def asLabelledNode(key: NodeShape): Labelled = grammars.create(key, new GrammarForAst(grammar).asNode(key))

    def manyVertical = new ManyVertical(addTriviaIfUseful(grammar, false))

    def ~(other: BiGrammar) = new LeftRight(grammar, addTriviaIfUseful(other))

    def many = new ManyHorizontal(addTriviaIfUseful(grammar))

    def %(bottom: BiGrammar) = new TopBottom(grammar, addTriviaIfUseful(bottom, false))

    override implicit def addSequenceMethods(grammar: BiGrammar): BiGrammarExtension = new BiGrammarExtension(grammar, grammars)
  }

  override def dependencies: Set[Contract] = Set.empty
}
