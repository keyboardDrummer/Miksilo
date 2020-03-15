package miksilo.modularLanguages.core.deltas

import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.bigrammar.grammars.As
import miksilo.modularLanguages.core.node.{NodeField, NodeGrammar, NodeShape}
import miksilo.editorParser.parsers.core.TextPointer
import miksilo.editorParser.parsers.editorParsers.OffsetPointerRange

class GrammarForAst(grammar: BiGrammar) {
  def asNode(key: NodeShape) = new NodeGrammar(grammar, key)
  def as(field: NodeField, changePosition: (TextPointer, TextPointer) => OffsetPointerRange = null) = As(grammar, field, changePosition)
}
