package core.transformation

import core.grammar.{FailureG, ToPackrat}

object ProgramGrammar

class TransformationsToPackrat extends ToPackrat {
  def buildParser(transformations: Seq[GrammarTransformation]): String => ParseResult[Any] = {
    val grammars: GrammarCatalogue = new GrammarCatalogue()
    grammars.create(ProgramGrammar, FailureG)
    for (transformation <- transformations) {
      transformation.transformDelimiters(lexical.delimiters)
      transformation.transformReserved(lexical.reserved)
      transformation.transformGrammars(grammars)
    }
    val packratParser = phrase(convert(grammars.find(ProgramGrammar)))
    input => packratParser(new PackratReader(new lexical.Scanner(input)))
  }

}