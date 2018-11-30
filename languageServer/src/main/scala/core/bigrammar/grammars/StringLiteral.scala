package core.bigrammar.grammars

import core.bigrammar.{BiGrammarToParser, WithMap}
import core.bigrammar.printer.TryState
import core.responsiveDocument.ResponsiveDocument

object StringLiteral extends StringGrammar {
  override def getParser(keywords: scala.collection.Set[String]): BiGrammarToParser.EditorParser[Any] = BiGrammarToParser.stringLiteral

  override def write(from: WithMap[Any]): TryState[ResponsiveDocument] = TryState.value("\"" + from.value + "\"")
}
