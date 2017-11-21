package core.bigrammar.grammars

import core.bigrammar.printer.{Printer, TryState}
import core.bigrammar.{BiGrammar, WithMapG}
import core.document.Empty

object ParseWhiteSpace extends CustomGrammarWithoutChildren with BiGrammarWithoutChildren {
  val regex = """\s+""".r

  override def getGrammar = {
    core.grammar.RegexG(regex)
  }

  override def write(from: WithMapG[Any]) =
    if (regex.replaceSomeIn(from.value.asInstanceOf[String], _ => Some("")).isEmpty) TryState.value(Empty)
    else Printer.fail(s"String ${from.value} was not whitespace")

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = true
}
