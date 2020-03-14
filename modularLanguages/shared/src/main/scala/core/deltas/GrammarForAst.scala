package core.deltas

import core.bigrammar.BiGrammar
import core.bigrammar.grammars.As
import core.language.node.{NodeField, NodeGrammar, NodeShape}
import miksilo.editorParser.parsers.TextPointer
import miksilo.editorParser.parsers.editorParsers.OffsetPointerRange

class GrammarForAst(grammar: BiGrammar) {
  def asNode(key: NodeShape) = new NodeGrammar(grammar, key)
  def as(field: NodeField, changePosition: (TextPointer, TextPointer) => OffsetPointerRange = null) = As(grammar, field, changePosition)
}
