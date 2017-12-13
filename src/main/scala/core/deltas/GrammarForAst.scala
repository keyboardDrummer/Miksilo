package core.deltas

import core.bigrammar.BiGrammar
import core.bigrammar.grammars.As
import core.deltas.node.{NodeShape, NodeField}

class GrammarForAst(grammar: BiGrammar) {
  def asNode(key: NodeShape) = new NodeGrammar(grammar, key)
  def as(field: NodeField) = As(grammar, field)
}
