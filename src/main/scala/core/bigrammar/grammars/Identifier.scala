package core.bigrammar.grammars

import core.bigrammar.BiGrammarToParser

case class Identifier(verifyWhenPrinting: Boolean = false) extends StringGrammar(verifyWhenPrinting) {
  override def getParser(keywords: Set[String]): BiGrammarToParser.Parser[String] =
    BiGrammarToParser.ident.filter(identifier =>
    !keywords.contains(identifier))
}
