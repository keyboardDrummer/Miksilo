package core.bigrammar.grammars

import core.parsers.editorParsers.{History, ParseError}
import core.bigrammar.BiGrammarToParser._

case class Identifier(verifyWhenPrinting: Boolean = false) extends StringGrammar(verifyWhenPrinting) {
  override def getParserBuilder(keywords: scala.collection.Set[String]): Parser[String] =
    parseIdentifier.filter(identifier => !keywords.contains(identifier),
      ready => s"$parseIdentifier is a keyword, and so can not be used as an identifier")
}
