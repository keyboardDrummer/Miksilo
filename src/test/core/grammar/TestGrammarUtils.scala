package core.grammar

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import core.transformation.GrammarTransformation

object TestGrammarUtils extends ToPackrat {

  def buildParser(transformations: Seq[GrammarTransformation]): String => ParseResult[Any] = {
    val packratParser = convert(grammar)
    input => packratParser(new PackratReader(new lexical.Scanner(input)))
  }
}
