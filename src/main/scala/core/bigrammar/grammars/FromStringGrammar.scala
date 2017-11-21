package core.bigrammar.grammars

import core.bigrammar.BiGrammar
import core.bigrammar.BiGrammarToGrammar.WithMap
import core.bigrammar.printer.{Printer, TryState}
import core.grammar.{Grammar, GrammarToParserConverter}

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

  override def write(from: WithMap) = {
    from.value match {
      case string: String =>
        if (verifyWhenPrinting) {
          val parseResult = parser(new CharArrayReader(string.toCharArray))
          if (parseResult.successful && parseResult.get.equals(from.value))
            TryState.value(string)
          else
            Printer.fail("FromGrammarWithToString could not parse string")
        }
        else
          TryState.value(string)

      case _ => Printer.fail(s"FromStringGrammar expects a string value, and not a $from")
    }
  }
}
