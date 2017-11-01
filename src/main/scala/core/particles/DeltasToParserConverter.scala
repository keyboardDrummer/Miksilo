package core.particles

import core.bigrammar._
import core.grammar.{GrammarToParserConverter, ParseException}
import core.particles.grammars.GrammarCatalogue

import scala.util.parsing.input.CharArrayReader

class DeltasToParserConverter extends GrammarToParserConverter {
  def buildParser(transformations: Seq[DeltaWithGrammar]): String => ParseResult[Any] = {
    val catalogue = new CompilerFromDeltas(transformations).language.grammarCatalogue
    buildParser(catalogue)
  }

  def parse(catalogue: GrammarCatalogue, input: String): Any = {
    val parser = buildParser(catalogue)

    val parseResult = parser(input)
    if (!parseResult.successful)
      throw ParseException(parseResult.toString)

    if(!parseResult.next.atEnd)
      throw ParseException("Did not parse until end.")

    parseResult.get
  }

  def buildParser(grammars: GrammarCatalogue): (String) => ParseResult[Any] = {
    val biGrammar = WithTrivia(new IgnoreRight(new Sequence(grammars.root, grammars.trivia)), grammars.trivia)
    val programGrammar = BiGrammarToGrammar.toGrammar(biGrammar)
    input => convert(programGrammar)(new CharArrayReader(input.toCharArray))
  }
}