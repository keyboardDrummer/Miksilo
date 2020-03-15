package miksilo.languageServer.core.language

import miksilo.languageServer.server.SourcePath
import miksilo.editorParser.parsers.{RealSourceElement, SourceElement}

case class SourcePathFromElement(uri: String, sourceElement: SourceElement) extends SourcePath {

  override def uriOption = Some(uri)

  override def childElements: Seq[SourcePathFromElement] = {
    sourceElement.childElements.map(e => SourcePathFromElement(uri, e))
  }

  override def rangeOption = sourceElement.rangeOption
}

trait FileElement extends RealSourceElement {

  def addFile(uri: String) = SourcePathFromElement(uri, this)
}
