package core.parsers.strings

import core.parsers._
import core.parsers.sequences.{ElemPredicate, SequenceParserWriter}
import langserver.types.Position
import languageServer.HumanPosition

import scala.util.matching.Regex
import scala.util.parsing.input.Positional

trait StringParserWriter extends SequenceParserWriter {
  type Elem = Char
  type Input = StringReader

  def position[T <: Positional]: Parser[Position] = new Parser[Position] {
    override def parseNaively(input: Input, state: ParseState): ParseResult[Position] = {
      ParseSuccess(new HumanPosition(input.position.line, input.position.column), input, NoFailure)
    }

    override def getDefault(cache: DefaultCache): Option[Position] = None
  }

  implicit def literal(value: String): Literal = Literal(value)
  implicit def regex(value: Regex): RegexParser = RegexParser(value)
}



