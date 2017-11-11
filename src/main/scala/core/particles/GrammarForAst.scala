package core.particles

import core.bigrammar.BiGrammar
import core.bigrammar.grammars.As
import core.particles.node.{NodeClass, NodeField}

class GrammarForAst(grammar: BiGrammar) {
  def asNode(key: NodeClass) = new NodeGrammar(grammar, key)
  def as(field: NodeField) = As(grammar, field)
}
