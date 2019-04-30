package core.bigrammar.grammars

import core.bigrammar.BiGrammarToParser

import scala.util.matching.Regex

import BiGrammarToParser._

case class RegexGrammar(regex: Regex, name: String, verifyWhenPrinting: Boolean = false)
  extends StringGrammar(verifyWhenPrinting) {
  override def getParser(keywords: scala.collection.Set[String]): EditorParser[Any] = BiGrammarToParser.regex(regex, name)

  override def hashCode(): Int = regex.toString().hashCode

  override def equals(obj: scala.Any): Boolean = obj match {
    case RegexGrammar(otherRegex, _, _) => regex.toString().equals(otherRegex.toString())
    case _ => false
  }
}
