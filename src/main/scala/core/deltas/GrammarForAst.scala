package core.deltas

import core.bigrammar.BiGrammar
import core.bigrammar.grammars.As
import core.deltas.node.{NodeClass, NodeField}

class GrammarForAst(grammar: BiGrammar) {
  def asNode(key: NodeClass) = new NodeGrammar(grammar, key)
  def as(field: NodeField) = As(grammar, field)
}
