package core.bigrammar.grammars

import core.bigrammar.printer.{Printer, TryState}
import core.bigrammar.{BiGrammar, BiGrammarToParser, WithMap}
import core.document.Empty
import core.responsiveDocument.ResponsiveDocument

import scala.util.matching.Regex

import BiGrammarToParser._

object ParseWhiteSpace extends CustomGrammarWithoutChildren with BiGrammarWithoutChildren {
  val regex: Regex = """\s+""".r

  override def getParser(keywords: scala.collection.Set[String]): EditorParser[Any] =
    RegexGrammar(regex, "whitespace").getParser(keywords)

  override def write(from: WithMap[Any]): TryState[ResponsiveDocument] =
    if (regex.replaceSomeIn(from.value.asInstanceOf[String], _ => Some("")).isEmpty) TryState.value(Empty)
    else Printer.fail(s"String ${from.value} was not whitespace")

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = true
}
