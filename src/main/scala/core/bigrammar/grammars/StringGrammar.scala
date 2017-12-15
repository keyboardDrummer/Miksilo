package core.bigrammar.grammars

import core.bigrammar.printer.{Printer, TryState}
import core.bigrammar.{BiGrammar, WithMapG}
import core.responsiveDocument.ResponsiveDocument

import scala.util.parsing.input.CharArrayReader

/**
  * Takes a grammar for parsing, and uses toString for printing.
  * so the result of the grammar is exactly what has been consumed.
  * verifyWhenPrinting When printing, make sure the string to print can be consumed by the grammar.
  */
abstract class StringGrammar(verifyWhenPrinting: Boolean = false)
  extends CustomGrammarWithoutChildren with BiGrammarWithoutChildren
{
  lazy val parser = getParser

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = true

  override def write(from: WithMapG[Any]): TryState[ResponsiveDocument] = {
    from.value match {
      case string: String =>
        if (verifyWhenPrinting) {
          val parseResult = parser(new CharArrayReader(string.toCharArray))
          if (parseResult.successful && parseResult.get.equals(from.value))
            TryState.value(string)
          else
            Printer.fail("StringGrammar could not parse string")
        }
        else
          TryState.value(string)

      case _ => Printer.fail(s"StringGrammar expects a string value, and not a $from")
    }
  }
}
