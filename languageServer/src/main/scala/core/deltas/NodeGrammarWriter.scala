package core.deltas

import core.bigrammar.{BiGrammar, BiGrammarWriter, RootGrammar}

object NodeGrammarWriter extends NodeGrammarWriter

trait NodeGrammarWriter extends BiGrammarWriter {
  implicit def grammarAsRoot(grammar: BiGrammar[_]): RootGrammar = new RootGrammar(grammar)
  implicit val postfixOps = language.postfixOps
}
