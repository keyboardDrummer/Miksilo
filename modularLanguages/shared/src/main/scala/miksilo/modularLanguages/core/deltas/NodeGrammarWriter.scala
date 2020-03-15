package miksilo.modularLanguages.core.deltas

import miksilo.modularLanguages.core.bigrammar.{BiGrammar, BiGrammarWriter, RootGrammar}

object NodeGrammarWriter extends NodeGrammarWriter
trait NodeGrammarWriter extends BiGrammarWriter {
  implicit def grammarAsRoot(grammar: BiGrammar): RootGrammar = new RootGrammar(grammar)
  implicit val postfixOps = language.postfixOps
  implicit def toAstGrammar(grammar: BiGrammar): GrammarForAst = new GrammarForAst(grammar)
}
