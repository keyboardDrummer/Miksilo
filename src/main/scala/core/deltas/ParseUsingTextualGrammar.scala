package core.deltas

import java.io.InputStream

import core.bigrammar.BiGrammarToParser
import core.deltas.node.Node

import scala.util.parsing.input.CharArrayReader

object ParseUsingTextualGrammar extends Delta {
  override def inject(language: Language): Unit = {

    language.buildParser = () => {

      val parser: BiGrammarToParser.PackratParser[Any] = BiGrammarToParser.toParser(language.grammars.root)

      def parse(input: InputStream): Node = {
        val reader = new CharArrayReader(scala.io.Source.fromInputStream(input).mkString.toCharArray)
        val parseResult: BiGrammarToParser.ParseResult[Any] = parser(reader)
        if (!parseResult.successful)
          throw ParseException(parseResult.toString)

        if(!parseResult.next.atEnd)
          throw ParseException("Did not parse until end.")

        parseResult.get.asInstanceOf[Node]
      }

      parse
    }
  }

  override def description: String = "Parses the input file using a textual grammar."

  override def dependencies: Set[Contract] = Set.empty
}
