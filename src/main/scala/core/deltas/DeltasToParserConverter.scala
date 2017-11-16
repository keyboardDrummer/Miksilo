package core.deltas

import core.bigrammar._
import core.grammar.{GrammarToParserConverter, ParseException}

import scala.util.parsing.input.CharArrayReader

class DeltasToParserConverter extends GrammarToParserConverter {
  def buildParser(transformations: Seq[DeltaWithGrammar]): String => ParseResult[Any] = {
    val language = new CompilerFromDeltas(transformations).language
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

  def buildParser(grammar: BiGrammar): (String) => ParseResult[Any] = {
    val programGrammar = BiGrammarToGrammar.toGrammar(grammar)
    input => convert(programGrammar)(new CharArrayReader(input.toCharArray))
  }
}