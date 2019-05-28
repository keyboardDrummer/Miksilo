package core.parsers.editorParsers

import core.language.node.SourceRange

trait ParseError[Input] {
  def canMerge: Boolean = false
  def penalty: Double
  def score: Double = -penalty * 1
  def append(other: ParseError[Input]): Option[ParseError[Input]] = None
  def message: String
  def from: Input
  def to: Input
  def range: SourceRange

  override def toString = message
}
