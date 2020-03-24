package miksilo.editorParser.languages.json

import miksilo.editorParser.parsers.editorParsers.{History, LeftRecursiveCorrectingParserWriter}
import miksilo.editorParser.parsers.strings.{CommonParserWriter, NoStateParserWriter, WhitespaceParserWriter}

import scala.collection.immutable.ListMap

object JsonParser extends CommonParserWriter
  with NoStateParserWriter
  with LeftRecursiveCorrectingParserWriter
  with WhitespaceParserWriter  {

  lazy val array = ("[" ~> valueParser.manySeparated(",", "value") ~< "]").
    withSourceRange((range, value) => JsonArray(Some(range), value.toArray))
  lazy val objectMember = stringLiteral ~< ":" ~ valueParser
  lazy val objectParser = (literal("{", 2 * History.missingInputPenalty) ~>
    objectMember.manySeparated(",", "member") ~< "}").
    withSourceRange((range, value) => JsonObject(Some(range), ListMap.from(value)))
  lazy val number = wholeNumber.withSourceRange((range, value) => NumberLiteral(Some(range), Integer.parseInt(value)))
  lazy val string = stringLiteral.withSourceRange((range, value) => StringLiteral(Some(range), value))
  lazy val hole = Fallback(RegexParser(" *".r, "spaces").withSourceRange((range,_) => ValueHole(Some(range))), "value")

  lazy val valueParser: Parser[JsonValue] = new Lazy(array | objectParser | number | string | hole)
  val parser = valueParser.getWholeInputParser()
}
