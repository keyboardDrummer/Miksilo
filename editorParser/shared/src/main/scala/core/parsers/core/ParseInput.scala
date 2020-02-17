package core.parsers.core

trait ParseInput[Input] {
  def drop(text: ParseText, amount: Int): Input
  def offset: Int
  def atEnd(array: ParseText): Boolean
}
