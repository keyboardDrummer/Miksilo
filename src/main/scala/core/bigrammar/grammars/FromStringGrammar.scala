package core.bigrammar.grammars

import core.bigrammar.BiGrammar
import core.bigrammar.BiGrammarToGrammar.WithMap
import core.bigrammar.printer.TryState
import core.bigrammar.printer.TryState.State
import core.grammar.{Grammar, GrammarToParserConverter}

import scala.util.Success
import scala.util.parsing.input.CharArrayReader

/**
  * Takes a grammar for parsing, and uses toString for printing.
  * so the result of the grammar is exactly what has been consumed.
  * verifyWhenPrinting When printing, make sure the string to print can be consumed by the grammar.
  */
class FromStringGrammar(val grammar: Grammar, verifyWhenPrinting: Boolean = false)
  extends CustomGrammarWithoutChildren with BiGrammarWithoutChildren
{
  override def getGrammar = grammar

  lazy val parser = GrammarToParserConverter.convert(grammar)

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = true

  override def write(from: WithMap, state: State) = {
    from.value match {
      case string: String =>
        if (verifyWhenPrinting) {
          val parseResult = parser(new CharArrayReader(string.toCharArray))
          if (parseResult.successful && parseResult.get.equals(from.value))
            Success(state, string)
          else
            TryState.fail("FromGrammarWithToString could not parse string")
        }
        else
          Success(state, string)

      case _ => TryState.fail(s"FromStringGrammar expects a string value, and not a $from")
    }
  }
}
