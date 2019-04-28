package core.parsers

import editorParsers.EditorParserWriter
import strings.CommonParserWriter
import langserver.types.Position
import strings.ScoredPosition

trait CommonStringReaderParser extends CommonParserWriter with EditorParserWriter {
  type Input = StringReader

  class StringReader(array: ArrayCharSequence, offset: Int, scoredPosition: ScoredPosition) extends StringReaderBase(array, offset, scoredPosition) {

    def this(value: String) {
      this(value.toCharArray, 0, ScoredPosition(0, Position(0, 0)))
    }

    def drop(amount: Int): StringReader = new StringReader(array, offset + amount, move(amount))
  }
}
