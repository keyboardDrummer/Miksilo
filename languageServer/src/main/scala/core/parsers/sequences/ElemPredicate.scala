package core.parsers.sequences

import core.parsers._

case class ElemPredicate[Input <: SequenceInput[Input, Elem], Elem](predicate: Elem => Boolean, kind: String)
  extends Parser[Input, Elem] {
  override def parseNaively(input: Input, cache: ParseState): ParseResult[Elem] = {
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


