package core.bigrammar.grammars

import core.bigrammar.BiGrammarToParser

import scala.util.matching.Regex
import core.bigrammar.BiGrammarToParser._
import core.parsers.editorParsers.History

case class RegexGrammar(regex: Regex, name: String, verifyWhenPrinting: Boolean = false,
                        defaultValue: Option[String] = None,
                        score: Double = History.successValue,
                        penaltyOption: Option[Double] = Some(History.missingInputPenalty),
                        allowDrop: Boolean = true)
  extends StringGrammar(verifyWhenPrinting) {

  override def getParserBuilder(keywords: scala.collection.Set[String]): Parser[Any] =
      BiGrammarToParser.parseRegex(regex, name, defaultValue, score, penaltyOption, allowDrop = allowDrop)
}
