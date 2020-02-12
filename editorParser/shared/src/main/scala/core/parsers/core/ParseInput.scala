package core.parsers.core

trait ParseInput[Input] {
  def drop(array: ArrayCharSequence, amount: Int): Input
  def offset: Int
  def atEnd(array: ArrayCharSequence): Boolean
}
