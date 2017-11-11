package core.particles

import core.bigrammar.grammars._
import core.bigrammar.{BiGrammar, BiGrammarSequenceMethodsExtension}
import core.particles.grammars.LanguageGrammars
import core.particles.node.NodeClass

class GrammarWithTrivia(val grammar: BiGrammar)(implicit grammars: LanguageGrammars) extends NodeGrammarWriter
  with BiGrammarSequenceMethodsExtension
{
  def addTriviaIfUseful(grammar: BiGrammar, horizontal: Boolean = true) =
    if (grammar.containsParser()) new WithTrivia(grammar, grammars.trivia, horizontal) else grammar

  def asLabelledNode(key: NodeClass): Labelled = grammars.create(key, new GrammarForAst(grammar).asNode(key))

  def manyVertical = new ManyVertical(addTriviaIfUseful(grammar, false))

  def ~(other: BiGrammar) = new Sequence(grammar, addTriviaIfUseful(other))

  def many = new ManyHorizontal(addTriviaIfUseful(grammar))

  def %(bottom: BiGrammar) = new TopBottom(grammar, addTriviaIfUseful(bottom, false))

  override implicit def addSequenceMethods(grammar: BiGrammar): GrammarWithTrivia = new GrammarWithTrivia(grammar)
}
