package core.bigrammar.grammars

import core.bigrammar.printer.TryState
import core.bigrammar.{BiGrammar, WithMapG}
import core.document.Empty

object ParseWhiteSpace extends CustomGrammarWithoutChildren with BiGrammarWithoutChildren {
  val regex = """\s+""".r

  override def getGrammar = {
    core.grammar.RegexG(regex)
  }

  override def write(from: WithMapG[Any]) =
    if (regex.replaceSomeIn(from.value.asInstanceOf[String], _ => Some("")).isEmpty) TryState.ret(Empty)
    else TryState.fail(new Exception(s"String ${from.value} was not whitespace"))

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = true
}
