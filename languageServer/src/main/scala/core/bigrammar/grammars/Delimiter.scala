package core.bigrammar.grammars

import core.bigrammar.{BiGrammar, BiGrammarToParser, WithMap}
import core.bigrammar.printer.{Printer, TryState}
import core.parsers.editorParsers.History
import core.responsiveDocument.ResponsiveDocument

case class Delimiter(value: String, penalty: Double = History.missingInputPenalty, allowDrop: Boolean = true)
  extends CustomGrammarWithoutChildren[Unit] {

  if (value.length == 0)
    throw new RuntimeException("value must have non-zero length")

  override def containsParser(recursive: BiGrammar[_] => Boolean): Boolean = true

  override def write(from: Unit): TryState[ResponsiveDocument] = TryState.value(value)

  override def getParserBuilder(keywords: scala.collection.Set[String]): BiGrammarToParser.Self[String] =
    BiGrammarToParser.literal(value, penalty, allowDrop)
}
