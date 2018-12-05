package core.parsers.sequences

import core.parsers.core.EditorParserWriter
import core.parsers.editorParsers.DefaultCache

trait SequenceParserWriter extends EditorParserWriter {
  type Elem
  type Input <: SequenceInput[Input, Elem]

  def elem(predicate: Elem => Boolean, kind: String) = ElemPredicate(predicate, kind)

  case class ElemPredicate(predicate: Elem => Boolean, kind: String) extends EditorParser[Elem] {
    override def parseNaively(input: Input, cache: PState): ParseResult[Elem] = {
      if (input.atEnd) {
        return failure(input, s"$kind expected but end of source found")
      }

      val char = input.head
      if (predicate(char)) {
        ParseSuccess(char, input.tail, NoFailure)
      }
      else
        failure(input, s"'$char' was not a $kind")
    }

    override def getDefault(cache: DefaultCache): Option[Elem] = None
  }

}
