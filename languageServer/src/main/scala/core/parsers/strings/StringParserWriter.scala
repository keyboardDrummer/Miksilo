package core.parsers.strings

import core.parsers._
import langserver.types.Position
import languageServer.HumanPosition

import scala.util.matching.Regex
import scala.util.parsing.input.Positional

trait StringParserWriter extends ParserWriter {
  type Input = StringReader

  def elem(predicate: Char => Boolean, kind: String) = new ElemPredicate[Input, Char](predicate, kind)

  def position[T <: Positional]: Parser[Position] = new Parser[Position] {
    override def parseNaively(input: Input, state: ParseState): ParseResult[Position] = {
      ParseSuccess(new HumanPosition(input.position.line, input.position.column), input, NoFailure)
    }

    override def getDefault(cache: DefaultCache): Option[Position] = None
  }

  implicit def literal(value: String): Literal = Literal(value)
  implicit def regex(value: Regex): RegexFrom = RegexFrom(value)
}



