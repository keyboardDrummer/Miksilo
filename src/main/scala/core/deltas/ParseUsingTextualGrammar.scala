package core.deltas

import java.io.InputStream

import core.bigrammar.BiGrammarToParser
import core.language.node.Node
import core.language.{Language, NoSourceException, ParseException}
import util.SourceUtils

import scala.util.{Failure, Success, Try}
import scala.util.parsing.input.CharArrayReader

object ParseUsingTextualGrammar extends Delta {
  override def inject(language: Language): Unit = {

    language.buildParser = () => {

      val parser: BiGrammarToParser.PackratParser[Any] = BiGrammarToParser.toParser(language.grammars.root)

      def parse(input: InputStream): Try[Node] = {
        val reader = new CharArrayReader(SourceUtils.streamToString(input).mkString.toCharArray)
        if (reader.source.length() == 0)
          return Failure(NoSourceException)

        val parseResult: BiGrammarToParser.ParseResult[Any] = parser(reader)
        if (!parseResult.successful)
          Failure(ParseException(parseResult.toString))
        else
          Success(parseResult.get.asInstanceOf[Node])
      }

      parse
    }
  }

  override def description: String = "Parses the input file using a textual grammar."

  override def dependencies: Set[Contract] = Set.empty
}
