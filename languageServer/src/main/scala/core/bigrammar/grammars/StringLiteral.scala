package core.bigrammar.grammars

import core.bigrammar.{BiGrammarToParser, WithMap}
import core.responsiveDocument.ResponsiveDocument
import BiGrammarToParser._
import core.bigrammar.BiGrammar.State
import core.bigrammar.printer.Printer.TryState

object StringLiteral extends StringGrammar {
  override def getParserBuilder(keywords: scala.collection.Set[String]): Self[Any] = stringLiteral

  override def write(from: WithMap[Any], state: State): TryState[ResponsiveDocument] = scala.util.Success(state, "\"" + from.value + "\"")
}
