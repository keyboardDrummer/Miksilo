package core.transformation

import core.grammar.{Keyword, Grammar, ToPackrat}
import core.grammarDocument
import core.grammarDocument.{FailureG, ToGrammar}
import core.transformation.grammars.{GrammarCatalogue, ProgramGrammar}
import core.transformation.sillyCodePieces.GrammarTransformation

import scala.util.parsing.input.CharArrayReader


class TransformationsToPackrat extends ToPackrat {
  def buildParser(transformations: Seq[GrammarTransformation]): String => ParseResult[Any] = {
    val grammars: GrammarCatalogue = new GrammarCatalogue()
    grammars.create(ProgramGrammar, FailureG)
    for (transformation <- transformations) {
      transformation.transformGrammars(grammars)
    }
    buildParser(grammars)
  }

  def buildParser(grammars: GrammarCatalogue): (String) => ParseResult[Any] = {
    val programGrammarDocument: grammarDocument.Labelled = grammars.find(ProgramGrammar)
    val programGrammar = ToGrammar.toGrammar(programGrammarDocument)
    val allGrammars: Set[Grammar] = programGrammar.getGrammars
    keywords ++= allGrammars.collect({ case keyword: Keyword => keyword.value})
    val packratParser = phrase(convert(programGrammar))
    input => packratParser(new CharArrayReader(input.toCharArray))
  }
}