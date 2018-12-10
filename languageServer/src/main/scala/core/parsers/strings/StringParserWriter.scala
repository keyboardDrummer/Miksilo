package core.parsers.strings

import core.parsers.editorParsers.DefaultCache
import core.parsers.sequences.SequenceParserWriter

import scala.util.matching.Regex

trait StringParserWriter extends SequenceParserWriter {
  type Elem = Char
  type Input = StringReader

  implicit def literalToExtensions(value: String): EditorParserExtensions[String] = Literal(value)
  implicit def literal(value: String): Literal = Literal(value)
  implicit def regex(value: Regex): RegexParser = RegexParser(value)

  case class Literal(value: String) extends EditorParser[String] {
    override def parse(inputs: StringReader, state: PState): PR[String] = {
      var index = 0
      val array = inputs.array
      while(index < value.length) {
        val arrayIndex = index + inputs.offset
        if (array.length <= arrayIndex) {
          return ParseFailure(Some(value), inputs, s"expected '$value' but end of source found")
        } else if (array.charAt(arrayIndex) != value.charAt(index)) {
          return ParseFailure(Some(value), inputs.drop(index), s"expected '$value' but found '${array.subSequence(inputs.offset, arrayIndex + 1)}'")
        }
        index += 1
      }
      ParseSuccess(value, inputs.drop(value.length), NoFailure)
    }

    override def getDefault(cache: DefaultCache): Option[String] = Some(value)
  }

  case class RegexParser(regex: Regex) extends EditorParser[String] {
    override def parse(inputs: StringReader, state: PState): PR[String] = {
      regex.findPrefixMatchOf(new SubSequence(inputs.array, inputs.offset)) match {
        case Some(matched) =>
          ParseSuccess(
            inputs.array.subSequence(inputs.offset, inputs.offset + matched.end).toString,
            inputs.drop(matched.end), NoFailure)
        case None =>
          val nextCharacter =
            if (inputs.array.length == inputs.offset) "end of source"
            else inputs.array.charAt(inputs.offset)
          ParseFailure(None, inputs, s"expected '$regex' but found '$nextCharacter'") // Partial regex matching toevoegen
      }
    }

    override def getDefault(cache: DefaultCache): Option[String] = None
  }
}
