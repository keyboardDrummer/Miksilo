package core.bigrammar.grammars

import core.bigrammar.WithMap
import core.bigrammar.printer.TryState
import core.responsiveDocument.ResponsiveDocument

object StringLiteral extends StringGrammar {
  override def getParser(keywords: scala.collection.Set[String]): Parser[Any] = stringLiteral

  override def write(from: WithMap[Any]): TryState[ResponsiveDocument] = TryState.value("\"" + from.value + "\"")
}
