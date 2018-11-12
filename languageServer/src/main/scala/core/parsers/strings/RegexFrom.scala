package core.parsers.strings

import core.parsers._

import scala.util.matching.Regex

case class RegexFrom(regex: Regex) extends Parser[StringReader, String] {
  override def parseNaively(inputs: StringReader, cache: ParseState): ParseResult[String] = {
    regex.findPrefixMatchOf(new SubSequence(inputs.array, inputs.offset)) match {
      case Some(matched) =>
        ParseSuccess(
          inputs.array.subSequence(inputs.offset, inputs.offset + matched.end).toString,
          inputs.drop(matched.end), NoFailure)        case None =>
        val nextCharacter =
          if (inputs.array.length == inputs.offset) "end of source"
          else inputs.array.charAt(inputs.offset)
        ParseFailure(None, inputs, s"expected '$regex' but found '$nextCharacter'") // Partial regex matching toevoegen
    }
  }

  override def getDefault(cache: DefaultCache): Option[String] = None
}
