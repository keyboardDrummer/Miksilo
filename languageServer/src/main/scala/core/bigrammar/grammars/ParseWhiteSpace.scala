package core.bigrammar.grammars

import core.bigrammar.BiGrammarToParser._
import core.bigrammar.printer.{Printer, TryState}
import core.bigrammar.{BiGrammar, BiGrammarToParser, WithMap}
import core.document.Empty
import core.responsiveDocument.ResponsiveDocument

import scala.util.matching.Regex

object ParseWhiteSpace extends CustomGrammarWithoutChildren with BiGrammarWithoutChildren {
  val regex: Regex = """\s+""".r

  override def getParserBuilder(keywords: scala.collection.Set[String]): Self[Any] =
    RegexGrammar(regex, "whitespace", score = 0, penaltyOption = None).getParserBuilder(keywords)

  override def write(from: WithMap[Any]): TryState[ResponsiveDocument] =
    if (regex.replaceSomeIn(from.value.asInstanceOf[String], _ => Some("")).isEmpty) TryState.value(Empty)
    else Printer.fail(s"String ${from.value} was not whitespace")

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = true
}
