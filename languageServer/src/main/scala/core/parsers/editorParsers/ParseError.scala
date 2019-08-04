package core.parsers.editorParsers

import languageServer.TextEdit

case class Fix(title: String, edit: TextEdit)

trait ParseError[Input] {
  def canMerge: Boolean = false
  def penalty: Double
  def fix: Option[Fix] = None
  def score: Double = -penalty * 1
  def append(other: ParseError[Input]): Option[ParseError[Input]] = None
  def message: String
  def from: Input
  def to: Input

  override def toString = s"$message AT $from"
}
