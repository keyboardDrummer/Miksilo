package core.bigrammar.grammars

import core.bigrammar.BiGrammarToParser

case class Identifier(verifyWhenPrinting: Boolean = false) extends StringGrammar(verifyWhenPrinting) {
  override def getParser(keywords: scala.collection.Set[String]): BiGrammarToParser.Processor[String] =
    BiGrammarToParser.identifier.filter(identifier => !keywords.contains(identifier),
      identifier => s"$identifier is a keyword, and so can not be used as an identifier")
}
