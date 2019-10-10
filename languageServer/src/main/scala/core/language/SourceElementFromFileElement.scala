package core.language

import languageServer.SourceRange

case class SourceElementFromFileElement(uri: String, element: FileSourceElement) extends SourceElement {
  override def range = Some(element.range)

  override def uriOption = Some(uri)

  override def childElements: Seq[SourceElementFromFileElement] = {
    element.childElements.map(e => SourceElementFromFileElement(uri, e))
  }
}

trait FileSourceElement {
  def range: SourceRange

  def childElements: Seq[FileSourceElement]
}
