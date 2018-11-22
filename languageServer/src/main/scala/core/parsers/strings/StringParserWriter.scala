package core.parsers.strings

import core.parsers._
import core.parsers.sequences.SequenceParserWriter
import langserver.types.Position
import languageServer.HumanPosition

import scala.util.matching.Regex
import scala.util.parsing.input.Positional

trait StringParserWriter extends SequenceParserWriter {
  type Elem = Char
  type Input = StringReader

  def position[T <: Positional]: Parser[Position] = StringPositionParser

  implicit def literal(value: String): Literal = Literal(value)
  implicit def regex(value: Regex): RegexParser = RegexParser(value)
}

object StringPositionParser extends Parser[StringReader, Position] {

  override def parseNaively(input: StringReader, state: ParseState): ParseResult[Position] = {
    ParseSuccess(new HumanPosition(input.position.line, input.position.column), input, NoFailure)
  }

  override def getDefault(cache: DefaultCache): Option[Position] = None

}
