package core.bigrammar.grammars

import core.bigrammar.printer.{Printer, TryState}
import core.bigrammar.{BiGrammar, BiGrammarToParser, WithMap}
import core.document.Empty

import scala.util.matching.Regex

object ParseWhiteSpace extends CustomGrammarWithoutChildren with BiGrammarWithoutChildren {
  val regex: Regex = """\s+""".r

  override def getParser(keywords: scala.collection.Set[String]): Parser[Any] =
    new RegexGrammar(regex).getParser(keywords)

  override def write(from: WithMap[Any]) =
    if (regex.replaceSomeIn(from.value.asInstanceOf[String], _ => Some("")).isEmpty) TryState.value(Empty)
    else Printer.fail(s"String ${from.value} was not whitespace")

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = true
}
