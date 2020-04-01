package miksilo.modularLanguages.core.bigrammar.grammars

import miksilo.modularLanguages.core.bigrammar.BiGrammarToParser._
import miksilo.modularLanguages.core.bigrammar.WithMap
import miksilo.modularLanguages.core.bigrammar.printer.TryState
import miksilo.editorParser.responsiveDocument.ResponsiveDocument

object StringLiteralGrammar extends StringGrammar {
  override def getParserBuilder(keywords: scala.collection.Set[String]): Parser[Any] = stringLiteral

  override def write(from: WithMap[Any]): TryState[ResponsiveDocument] = TryState.value("\"" + from.value + "\"")
}
