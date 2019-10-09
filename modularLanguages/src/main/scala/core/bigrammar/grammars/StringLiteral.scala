package core.bigrammar.grammars

import core.responsiveDocument.ResponsiveDocument
import BiGrammarToParser._

object StringLiteral extends StringGrammar {
  override def getParserBuilder(keywords: scala.collection.Set[String]): Self[Any] = stringLiteral

  override def write(from: WithMap[Any]): TryState[ResponsiveDocument] = TryState.value("\"" + from.value + "\"")
}
