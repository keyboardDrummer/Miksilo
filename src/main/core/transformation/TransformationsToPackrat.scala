package core.transformation

import core.grammar.{Grammar, Keyword, ToPackrat}
import core.grammarDocument.{FailureG, Labelled, GrammarDocumentToGrammar}
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
    val allGrammars: Set[Grammar] = programGrammar.getGrammars
    keywords ++= allGrammars.collect({ case keyword: Keyword => keyword.value})
    val packratParser = phrase(convert(programGrammar))
    input => packratParser(new CharArrayReader(input.toCharArray))
  }
}