package core.bigrammar.grammars

import core.bigrammar.{BiGrammar, BiGrammarToParser, WithMap}
import core.bigrammar.printer.{Printer, TryState}
import core.document.Empty
import core.responsiveDocument.ResponsiveDocument
import BiGrammarToParser._
import scala.util.matching.Regex

object ParseWhiteSpace extends CustomGrammarWithoutChildren with BiGrammarWithoutChildren {
  val regex: Regex = """\s+""".r

  override def getParserBuilder(keywords: scala.collection.Set[String]): Self[Any] =
    parseRegex(regex, "whitespace", score = -0.001,
      penaltyOption = None, // Do not allow insertion
      allowDrop = false)

  override def write(from: WithMap[Any]): TryState[ResponsiveDocument] =
    if (regex.replaceSomeIn(from.value.asInstanceOf[String], _ => Some("")).isEmpty) TryState.value(Empty)
    else Printer.fail(s"String ${from.value} was not whitespace")

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = true
}
