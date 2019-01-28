package core.bigrammar.grammars

import core.bigrammar.BiGrammarToParser._
import core.bigrammar.printer.{Printer, TryState}
import core.bigrammar.{BiGrammar, WithMap}
import core.responsiveDocument.ResponsiveDocument

/**
  * Takes a grammar for parsing, and uses toString for printing.
  * so the result of the grammar is exactly what has been consumed.
  * verifyWhenPrinting When printing, make sure the string to print can be consumed by the grammar.
  */
abstract class StringGrammar(verifyWhenPrinting: Boolean = false)
  extends CustomGrammarWithoutChildren with BiGrammarWithoutChildren
{
  lazy val parser = getParser(Set.empty) //TODO hacky Set.empty

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = true

  override def write(from: WithMap[Any]): TryState[ResponsiveDocument] = {
    from.value match {
      case string: String =>
        if (verifyWhenPrinting) {
          parser.parseWholeInput(new Reader(string)).successOption match {
            case Some(success) if success.result.equals(from.value) => TryState.value(string)
            case _ => Printer.fail("StringGrammar could not parse string")
          }
        }
        else
          TryState.value(string)

      case _ =>
        Printer.fail(s"StringGrammar expects a string value, and not a ${from.value}")
    }
  }
}
