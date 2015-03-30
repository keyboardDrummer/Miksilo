package core.particles

import core.bigrammar.{BiGrammarToGrammar, Labelled}
import core.grammar.{ParseException, GrammarToParserConverter}
import core.particles.grammars.{GrammarCatalogue, ProgramGrammar}

import scala.util.parsing.input.CharArrayReader

class ParticlesToParserConverter extends GrammarToParserConverter {
  def buildParser(transformations: Seq[ParticleWithGrammar]): String => ParseResult[Any] = {
    val programGrammar: Labelled = new CompilerFromParticles(transformations).getGrammar
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
    val programGrammar = BiGrammarToGrammar.toGrammar(programGrammarDocument)
    input => convert(programGrammar)(new CharArrayReader(input.toCharArray))
  }
}