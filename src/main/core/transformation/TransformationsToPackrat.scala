package core.transformation

import core.grammar._
import core.transformation.grammars.{GrammarCatalogue, ProgramGrammar}
import core.transformation.sillyCodePieces.GrammarTransformation


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
    val allGrammars: Set[Grammar] = grammars.getGrammars
    lexical.delimiters ++= allGrammars.collect({ case delimiter: Delimiter => delimiter.value})
    lexical.reserved ++= allGrammars.collect({ case keyword: Keyword => keyword.value})
    val packratParser = phrase(convert(grammars.find(ProgramGrammar)))
    input => packratParser(new PackratReader(new lexical.Scanner(input)))
  }
}