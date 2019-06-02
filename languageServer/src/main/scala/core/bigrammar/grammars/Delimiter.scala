package core.bigrammar.grammars

import core.bigrammar.{BiGrammar, BiGrammarToParser}

case class Delimiter(value: String) extends StringGrammar {
  if (value.length == 0)
    throw new RuntimeException("value must have non-zero length")

  override def getParserBuilder(keywords: scala.collection.Set[String]): BiGrammarToParser.Self[String] =
    BiGrammarToParser.literalOrKeyword(value)
}
