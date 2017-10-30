package core.particles

import core.bigrammar._
import core.grammar.{GrammarToParserConverter, ParseException}

import scala.util.parsing.input.CharArrayReader

class DeltasToParserConverter extends GrammarToParserConverter {
  def buildParser(transformations: Seq[DeltaWithGrammar]): String => ParseResult[Any] = {
    val programGrammar: Labelled = new CompilerFromDeltas(transformations).getGrammar
    buildParser(programGrammar)
  }

  def parse(grammar: Labelled, input: String): Any = {
    val parser = buildParser(grammar)

    val parseResult = parser(input)
    if (!parseResult.successful)
      throw new ParseException(parseResult.toString)

    if(!parseResult.next.atEnd)
      throw new ParseException("Did not parse until end.")

    parseResult.get
  }

  def buildParser(programGrammarDocument: Labelled): (String) => ParseResult[Any] = {
    val biGrammar = WithWhiteSpace(new IgnoreRight(new Sequence(programGrammarDocument, ParseWhiteSpace)))
    val programGrammar = BiGrammarToGrammar.toGrammar(biGrammar)
    input => convert(programGrammar)(new CharArrayReader(input.toCharArray))
  }
}