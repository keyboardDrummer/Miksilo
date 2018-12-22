package core.parsers.editorParsers

import core.parsers.core.ParserWriter

trait EditorParserWriter extends ParserWriter {

  type ExtraState = DefaultCache
  type Self[+Result] = EditorParser[Result]

  trait EditorParser[+Result] extends Parser[Result] with HasGetDefault[Result] {
    final def getDefault(state: ParseStateLike): Option[Result] = getDefault(state.extraState)
  }

  def failure[Result](partial: Result, input: Input, message: String): ParseResult[Result]
}
