package core.language

import core.parsers.editorParsers.{OffsetNodeRange, OffsetRange, SourceRange}

case class SourceElementFromFileElement(uri: String, element: FileElement) extends SourceElement {
  override def range = Some(element.range)

  override def uriOption = Some(uri)

  override def childElements: Seq[SourceElementFromFileElement] = {
    element.childElements.map(e => SourceElementFromFileElement(uri, e))
  }
}

trait FileElement {
  def range: OffsetNodeRange

  def childElements: Seq[FileElement]

  def addFile(uri: String) = SourceElementFromFileElement(uri, this)
}
