package core.bigrammar.grammars

import core.bigrammar.printer.{Printer, TryState}
import core.bigrammar.{BiGrammar, BiGrammarToParser, WithMapG}
import core.document.Empty

import scala.util.matching.Regex

object ParseWhiteSpace extends CustomGrammarWithoutChildren with BiGrammarWithoutChildren {
  val regex: Regex = """\s+""".r

  override def getParser(keywords: Set[String]): BiGrammarToParser.Parser[Any] =
    new RegexGrammar(regex).getParser(keywords)

  override def write(from: WithMapG[Any]) =
    if (regex.replaceSomeIn(from.value.asInstanceOf[String], _ => Some("")).isEmpty) TryState.value(Empty)
    else Printer.fail(s"String ${from.value} was not whitespace")

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = true
}
