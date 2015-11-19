package application.compilerCockpit

import core.grammar.Grammar
import org.fife.ui.rsyntaxtextarea.{TokenMaker, TokenMakerFactory}

class TokenMakerFactoryFromGrammar(grammar: Grammar) extends TokenMakerFactory
{
  override def getTokenMakerImpl(key: String): TokenMaker = new TokenMakerFromGrammar(grammar)
  override def keySet(): java.util.Set[String] = new java.util.HashSet[String]()
}
