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
  extends CustomGrammarWithoutChildren[String] with BiGrammarWithoutChildren[String]
{
  lazy val parser = getParserBuilder(Set.empty).getWholeInputParser //TODO hacky Set.empty

  override def containsParser(recursive: BiGrammar[_] => Boolean): Boolean = true

  override def write(from: String): TryState[ResponsiveDocument] = {
    from match {
      case string: String =>
        if (verifyWhenPrinting) {
          val parseResult = parser.parse(new Reader(string))
          parseResult.resultOption match {
            case Some(result) if parseResult.successful && result.equals(from) => TryState.value(string)
            case _ => Printer.fail("StringGrammar could not parse string")
          }
        }
        else
          TryState.value(string)

      case _ =>
        Printer.fail(s"StringGrammar expects a string value, and not a ${from}")
    }
  }
}
