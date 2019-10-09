package core.language

import core.language.node.FilePosition
import core.parsers.strings.SourceRange

case class FileRange(uri: String, range: SourceRange) {
  def contains(filePosition: FilePosition): Boolean = {
    uri == filePosition.uri && range.contains(filePosition.position)
  }
}

trait SourceElement {
  def current: Any
  /*
  A None value means the element is not part of the source.
   */
  def range: Option[SourceRange]

  /*
  A None value means the element is not part of the source.
   */
  def fileRange: Option[FileRange] = range.flatMap(r => uriOption.map(u => new FileRange(u, r)))

  def uriOption: Option[String]

  def isOutsideFile(uri: String): Boolean = {
    uriOption match {
      case Some(nodeUri) => uri != nodeUri
      case None => false
    }
  }
}
