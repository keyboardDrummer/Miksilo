package core.bigrammar.grammars

import core.bigrammar.printer.TryState.State
import core.bigrammar.{BiGrammar, WithMapG}
import core.document.Empty

import scala.util.{Failure, Success}

object ParseWhiteSpace extends CustomGrammarWithoutChildren with BiGrammarWithoutChildren {
  val regex = """\s+""".r

  override def getGrammar = {
    core.grammar.RegexG(regex)
  }

  override def write(from: WithMapG[Any], state: State) =
    if (regex.replaceSomeIn(from.value.asInstanceOf[String], _ => Some("")).isEmpty) Success(state, Empty)
    else Failure(new Exception(s"String ${from.value} was not whitespace"))

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = true
}
