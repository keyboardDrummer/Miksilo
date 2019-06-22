package core.bigrammar.grammars

import core.bigrammar.BiGrammarToParser

import scala.util.matching.Regex
import BiGrammarToParser._
import core.parsers.editorParsers.History

case class RegexGrammar(regex: Regex, name: String, verifyWhenPrinting: Boolean = false, score: Double = History.successValue)
  extends StringGrammar(verifyWhenPrinting) {
  override def getParserBuilder(keywords: scala.collection.Set[String]): Self[Any] =
    BiGrammarToParser.regex(regex, name, score)

  override def hashCode(): Int = regex.toString().hashCode

  override def equals(obj: scala.Any): Boolean = obj match {
    case RegexGrammar(otherRegex, _, _, _) => regex.toString().equals(otherRegex.toString())
    case _ => false
  }
}
