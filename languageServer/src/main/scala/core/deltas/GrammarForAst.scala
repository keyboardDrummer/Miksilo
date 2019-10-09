package core.deltas

import core.bigrammar.BiGrammar
import core.bigrammar.grammars.As
import core.language.node.{NodeField, NodeGrammar, NodeShape}
import core.parsers.strings.SourceRange

class GrammarForAst(grammar: BiGrammar) {
  def asNode(key: NodeShape) = new NodeGrammar(grammar, key)
  def as(field: NodeField, changePosition: SourceRange => SourceRange = null) = As(grammar, field, changePosition)
}
