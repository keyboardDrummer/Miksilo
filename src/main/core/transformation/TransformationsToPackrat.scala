package core.transformation

import core.grammar.ToPackrat
import core.grammarDocument.{FailureG, GrammarDocumentToGrammar, Labelled}
import core.transformation.grammars.{GrammarCatalogue, ProgramGrammar}
import core.transformation.sillyCodePieces.GrammarTransformation

import scala.util.parsing.input.CharArrayReader

object GrammarDocumentUtil {

  def getGrammarFromTransformations(transformations: Seq[GrammarTransformation]): Labelled = {
    val grammars: GrammarCatalogue = new GrammarCatalogue()
    grammars.create(ProgramGrammar, FailureG)
    for (transformation <- transformations) {
      transformation.transformGrammars(grammars)
    }
    val programGrammar = grammars.find(ProgramGrammar)
    programGrammar
  }
}

class TransformationsToPackrat extends ToPackrat {
  def buildParser(transformations: Seq[GrammarTransformation]): String => ParseResult[Any] = {
    val programGrammar: Labelled = GrammarDocumentUtil.getGrammarFromTransformations(transformations)
    buildParser(programGrammar)
  }

  def buildParser(grammars: GrammarCatalogue): (String) => ParseResult[Any] = {
    val programGrammarDocument: Labelled = grammars.find(ProgramGrammar)
    buildParser(programGrammarDocument)
  }

  def buildParser(programGrammarDocument: Labelled): (String) => ParseResult[Any] = {
    val programGrammar = GrammarDocumentToGrammar.toGrammar(programGrammarDocument)
    input => convert(programGrammar)(new CharArrayReader(input.toCharArray))
  }
}