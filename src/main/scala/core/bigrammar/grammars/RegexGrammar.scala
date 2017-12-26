package core.bigrammar.grammars

import core.bigrammar.BiGrammarToParser

import scala.util.matching.Regex

class RegexGrammar(val regex: Regex, verifyWhenPrinting: Boolean = false)
  extends StringGrammar(verifyWhenPrinting) {
  override def getParser(keywords: Set[String]): BiGrammarToParser.Parser[Any] = BiGrammarToParser.regex(regex)
}
