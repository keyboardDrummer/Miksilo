package core.parsers.editorParsers

trait ParseError[Input] {
  def canMerge: Boolean = false
  def penalty: Double
  def score: Double = -penalty * 1
  def append(other: ParseError[Input]): Option[ParseError[Input]] = None
  def message: String
  def from: Input
  def to: Input

  override def toString = s"$message AT $from"
}
