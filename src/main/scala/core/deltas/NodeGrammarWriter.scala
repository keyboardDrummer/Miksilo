package core.deltas

import core.bigrammar.{BiGrammar, BiGrammarWriter, RootGrammar}

trait NodeGrammarWriter extends BiGrammarWriter {

  implicit def grammarAsRoot(grammar: BiGrammar): RootGrammar = new RootGrammar(grammar)
  implicit val postfixOps = language.postfixOps
  implicit def toAstGrammar(grammar: BiGrammar): GrammarForAst = new GrammarForAst(grammar)
}
