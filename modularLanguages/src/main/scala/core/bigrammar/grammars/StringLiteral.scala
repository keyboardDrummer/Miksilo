package core.bigrammar.grammars

import core.responsiveDocument.ResponsiveDocument
import core.bigrammar.BiGrammarToParser._
import core.bigrammar.WithMap
import core.bigrammar.printer.TryState

object StringLiteral extends StringGrammar {
  override def getParserBuilder(keywords: scala.collection.Set[String]): Self[Any] = stringLiteral

  override def write(from: WithMap[Any]): TryState[ResponsiveDocument] = TryState.value("\"" + from.value + "\"")
}
