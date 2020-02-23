package core.language

import core.parsers.editorParsers.{FileOffsetRange, OffsetRange, SourceRange}
import lsp.{FileOffset, FilePosition, FileRange}

trait SourceElement {
  /*
  A None value means the element is not part of the source.
   */
  def range: Option[OffsetRange]

  def childElements: Seq[SourceElement] = Seq.empty

  /*
  A None value means the element is not part of the source.
   */
  def fileRange: Option[FileOffsetRange] = range.flatMap(p => uriOption.map(u => FileOffsetRange(u, p)))

  def uriOption: Option[String]

  def isInAnotherFile(uri: String): Boolean = {
    uriOption match {
      case Some(nodeUri) => uri != nodeUri
      case None => false
    }
  }

  def getChildForPosition(filePosition: FileOffset): Option[SourceElement] = {
    if (isInAnotherFile(filePosition.uri))
      return None

    if (!range.exists(r => r.contains(filePosition.offset)))
      return None

    val childResults = childElements.flatMap(child => child.getChildForPosition(filePosition))
    Some(childResults.headOption.getOrElse({
      this
    }))
  }
}
