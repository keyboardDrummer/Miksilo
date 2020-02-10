package core.parsers.core

trait ParseInput {
  def offset: Int
  def atEnd(array: ArrayCharSequence): Boolean
}
