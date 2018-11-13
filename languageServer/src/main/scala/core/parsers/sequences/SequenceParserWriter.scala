package core.parsers.sequences

import core.parsers.ParserWriter

trait SequenceParserWriter extends ParserWriter {
  type Elem
  type Input <: SequenceInput[Input, Elem]

  def elem(predicate: Elem => Boolean, kind: String) = new ElemPredicate[Input, Elem](predicate, kind)
}
