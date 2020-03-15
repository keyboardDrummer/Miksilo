package miksilo.editorParser.parsers

import editorParsers.OffsetPointerRange

trait RealSourceElement extends SourceElement {
  def range: OffsetPointerRange
  def rangeOption: Option[OffsetPointerRange] = Some(range)
}

trait SourceElement {
  /*
  A None value means the element is not part of the source.
   */
  def rangeOption: Option[OffsetPointerRange]

  def childElements: Seq[SourceElement] = Seq.empty
}
