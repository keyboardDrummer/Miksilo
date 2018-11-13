package core.bigrammar.grammars

import scala.util.matching.Regex

case class RegexGrammar(regex: Regex, verifyWhenPrinting: Boolean = false)
  extends StringGrammar(verifyWhenPrinting) {
  override def getParser(keywords: scala.collection.Set[String]): Parser[Any] = regex(regex)

  override def hashCode(): Int = regex.toString().hashCode

  override def equals(obj: scala.Any): Boolean = obj match {
    case RegexGrammar(otherRegex, _) => regex.toString().equals(otherRegex.toString())
    case _ => false
  }
}
