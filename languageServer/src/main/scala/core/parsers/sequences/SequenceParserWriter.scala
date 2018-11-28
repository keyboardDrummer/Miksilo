package core.parsers.sequences

import core.parsers._

trait SequenceParserWriter extends ParserWriter {
  type Elem
  type Input <: SequenceInput[Input, Elem]

  def elem(predicate: Elem => Boolean, kind: String) = new ElemPredicate(predicate, kind)

  case class ElemPredicate(predicate: Elem => Boolean, kind: String) extends Processor[Elem] {
    override def parseNaively(input: Input, cache: ParseState): PR[Elem] = {
      if (input.atEnd) {
        return ParseFailure(None, input, s"$kind expected but end of source found")
      }

      val char = input.head
      if (predicate(char)) {
        ParseSuccess(char, input.tail, NoFailure)
      }
      else
        ParseFailure(None, input, s"'$char' was not a $kind")
    }

    override def getDefault(cache: DefaultCache): Option[Elem] = None
  }

}
