package core.parsers.strings

import core.parsers
import core.parsers._

case class Literal(value: String) extends Parser[StringReader, String] {
  override def parse(inputs: StringReader, cache: ParseState): ParseResult[String] = {
    var index = 0
    val array = inputs.array
    while(index < value.length) {
      val arrayIndex = index + inputs.offset
      if (array.length <= arrayIndex) {
        return ParseFailure(Some(value), inputs, s"expected '$value' but end of source found")
      } else if (array.charAt(arrayIndex) != value.charAt(index)) {
        return parsers.ParseFailure(Some(value), inputs.drop(index), s"expected '$value' but found ${array.subSequence(inputs.offset, arrayIndex)}")
      }
      index += 1
    }
    ParseSuccess(value, inputs.drop(value.length), NoFailure)
  }

  override def getDefault(cache: DefaultCache): Option[String] = Some(value)
}
