package core.language

import core.language.node.{FileRange, SourceRange}

trait SourceElement {
  def current: Any
  /*
  A None value means the element is not part of the source.
   */
  def range: Option[SourceRange]

  /*
  A None value means the element is not part of the source.
   */
  def fileRange: Option[FileRange] = range.flatMap(p => uriOption.map(u => FileRange(u, p)))

  def uriOption: Option[String]

  def isOutsideFile(uri: String): Boolean = {
    uriOption match {
      case Some(nodeUri) => uri != nodeUri
      case None => false
    }
  }
}
