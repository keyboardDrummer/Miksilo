package core.particles

import core.biGrammar.{BiGrammarToGrammar, Labelled}
import core.grammar.GrammarToParserConverter
import core.particles.grammars.{GrammarCatalogue, ProgramGrammar}

import scala.util.parsing.input.CharArrayReader

class ParticlesToParserConverter extends GrammarToParserConverter {
  def buildParser(transformations: Seq[ParticleWithGrammar]): String => ParseResult[Any] = {
    val programGrammar: Labelled = new CompilerFromParticles(transformations).getGrammar
    buildParser(programGrammar)
  }

  def buildParser(grammars: GrammarCatalogue): (String) => ParseResult[Any] = {
    val programGrammarDocument: Labelled = grammars.find(ProgramGrammar)
    buildParser(programGrammarDocument)
  }

  def buildParser(programGrammarDocument: Labelled): (String) => ParseResult[Any] = {
    val programGrammar = BiGrammarToGrammar.toGrammar(programGrammarDocument)
    input => convert(programGrammar)(new CharArrayReader(input.toCharArray))
  }
}