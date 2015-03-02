package core.transformation

import core.grammar.ToPackrat
import core.grammarDocument.{BiGrammarToGrammar, Labelled}
import core.transformation.grammars.{GrammarCatalogue, ProgramGrammar}
import core.transformation.sillyCodePieces.GrammarTransformation

import scala.util.parsing.input.CharArrayReader

class TransformationsToPackrat extends ToPackrat {
  def buildParser(transformations: Seq[GrammarTransformation]): String => ParseResult[Any] = {
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