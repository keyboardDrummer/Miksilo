package core.parsers.sequences

import core.parsers.core.ParseInput
import core.parsers.editorParsers.{CorrectingParserWriter, History}

trait SequenceParserWriter extends CorrectingParserWriter {
  type Elem
  type Input <: SequenceInput[Input, Elem]

  trait SequenceInput[Input, Elem] extends EditorParseInput {
    def head: Elem
    def tail: Input
  }

  def elem(predicate: Elem => Boolean, kind: String) = ElemPredicate(predicate, kind)
  case class ElemPredicate(predicate: Elem => Boolean, kind: String)
    extends EditorParserBase[Elem] with LeafParser[Elem] {

    override def getParser(recursive: GetParse): Parse[Elem] = {

      def apply(input: Input, state: ParseState): ParseResult[Elem] = {
        if (input.atEnd) {
          return newFailure(input, s"$kind expected but end of source found")
        }

        val char = input.head
        if (predicate(char)) {
          newSuccess(char, input.tail, History.successValue)
        }
        else
          newFailure(input, s"'$char' was not a $kind")
      }

      apply
    }

    override def getMustConsume(cache: ConsumeCache) = true
  }

}
