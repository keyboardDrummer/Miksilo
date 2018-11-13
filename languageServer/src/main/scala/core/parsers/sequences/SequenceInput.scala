package core.parsers.sequences

import core.parsers.ParseInput

trait SequenceInput[Input, Elem] extends ParseInput {
  def head: Elem
  def tail: Input
}
