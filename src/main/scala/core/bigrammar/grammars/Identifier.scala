package core.bigrammar.grammars

import core.bigrammar.BiGrammarToParser

case class Identifier(verifyWhenPrinting: Boolean = false) extends StringGrammar(verifyWhenPrinting) {
  override def getParser = BiGrammarToParser.ident.filter(identifier =>
    !BiGrammarToParser.keywords.contains(identifier))
}
