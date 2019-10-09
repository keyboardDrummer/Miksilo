package core.parsers.editorParsers

import languageServer.TextEdit

case class Fix(title: String, edit: TextEdit)

trait ExternalParseError[+Input] {
  def fix: Option[Fix] = None
  def message: String
  def from: Input
  def to: Input
}

trait ParseError[Input] extends ExternalParseError[Input] {
  def canMerge: Boolean = false
  def penalty: Double
  def score: Double = -penalty * 1
  def append(other: ParseError[Input]): Option[ParseError[Input]] = None

  override def toString = s"$message AT $from"
}
