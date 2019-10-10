//package core.bigrammar
//
//import core.bigrammar.grammars._
//import core.deltas.grammars.TriviasGrammar
//
//trait WhitespaceTriviaSequenceCombinators extends AbstractGrammarWriter[BiGrammar] {
//
//  val trivia = new Labelled(TriviasGrammar, new ManyHorizontal(ParseWhiteSpace))
//
//  def addTriviaIfUseful(grammar: BiGrammar, horizontal: Boolean) =
//    if (grammar.containsParser()) new WithTrivia(grammar, trivia, horizontal) else grammar
//
//  implicit def stringAsGrammar(value: String): BiGrammarExtension = new BiGrammarExtension(value)
//  implicit class BiGrammarExtension(val grammar: BiGrammar) extends AbstractGrammarWriter {
//
//    def manyVertical = new ManyVertical(addTriviaIfUseful(grammar, horizontal = false))
//
//    override def sequence(other: BiGrammar, bijective: SequenceBijective, horizontal: Boolean): BiGrammar =
//      new BiSequence(grammar, addTriviaIfUseful(other, horizontal), bijective, horizontal)
//
//    def many = new ManyHorizontal(addTriviaIfUseful(grammar, horizontal = true))
//
//    override implicit def addSequenceMethods(grammar: BiGrammar): AbstractGrammarWriter = new BiGrammarExtension(grammar)
//  }
//}
