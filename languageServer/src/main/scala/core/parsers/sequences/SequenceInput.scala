package core.parsers.sequences

import core.parsers.editorParsers.CorrectingInput

trait SequenceInput[Input, Elem] extends CorrectingInput {
  def head: Elem
  def tail: Input
}
