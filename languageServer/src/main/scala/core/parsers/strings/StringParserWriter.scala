package core.parsers.strings

import core.parsers.sequences.SequenceParserWriter
import scala.util.matching.Regex

trait StringParserWriter extends SequenceParserWriter {
  type Elem = Char
  type Input = StringReader

  implicit def literal(value: String): Literal = Literal(value)
  implicit def regex(value: Regex): RegexParser = RegexParser(value)
}
