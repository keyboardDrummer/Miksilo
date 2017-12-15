package core.deltas

import core.bigrammar._
import core.grammar.{GrammarToParserConverter, ParseException}

import scala.util.parsing.input.CharArrayReader

class DeltasToParserConverter extends GrammarToParserConverter {
  def buildParser(deltas: Seq[DeltaWithGrammar]): String => BiGrammarToParser.ParseResult[Any] = {
    val language = new Language(deltas)
    buildParser(language.grammars.root)
  }

  def parse(grammar: BiGrammar, input: String): Any = {
    val parser = buildParser(grammar)

    val parseResult = parser(input)
    if (!parseResult.successful)
      throw ParseException(parseResult.toString)

    if(!parseResult.next.atEnd)
      throw ParseException("Did not parse until end.")

    parseResult.get
  }

  def buildParser(grammar: BiGrammar): (String) => BiGrammarToParser.ParseResult[Any] = {
    input => BiGrammarToParser.toParser(grammar)(new CharArrayReader(input.toCharArray))
  }
}