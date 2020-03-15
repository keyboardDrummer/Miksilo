package miksilo.modularLanguages.core.bigrammar.grammars

import miksilo.modularLanguages.core.bigrammar.{BiGrammar, BiGrammarToParser, WithMap}
import miksilo.modularLanguages.core.bigrammar.printer.{Printer, TryState}
import miksilo.editorParser.document.Empty
import BiGrammarToParser._
import miksilo.editorParser.responsiveDocument.ResponsiveDocument

import scala.util.matching.Regex

object ParseWhiteSpace extends CustomGrammarWithoutChildren with BiGrammarWithoutChildren {
  val regex: Regex = """\s+""".r

  override def getParserBuilder(keywords: scala.collection.Set[String]): Parser[Any] =
    parseRegex(regex, "whitespace", score = -0.001,
      penaltyOption = None, // Do not allow insertion
      allowDrop = false)

  override def write(from: WithMap[Any]): TryState[ResponsiveDocument] =
    if (regex.replaceSomeIn(from.value.asInstanceOf[String], _ => Some("")).isEmpty) TryState.value(Empty)
    else Printer.fail(s"String ${from.value} was not whitespace")

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = true
}
