package miksilo.languageServer.server

import miksilo.editorParser.parsers.editorParsers.{FileOffsetRange, OffsetPointerRange}
import miksilo.lspprotocol.lsp.FileOffset

trait SourcePath {
  def uriOption: Option[String]
  def fileRange: Option[FileOffsetRange] = rangeOption.flatMap(p => uriOption.map(u => FileOffsetRange(u, p.toOffsetRange)))
  def childElements: Seq[SourcePath] = Seq.empty
  def rangeOption: Option[OffsetPointerRange]

  def isInAnotherFile(uri: String): Boolean = {
    uriOption match {
      case Some(nodeUri) => uri != nodeUri
      case None => false
    }
  }

  def getChildForPosition(filePosition: FileOffset): Option[SourcePath] = {
    if (isInAnotherFile(filePosition.uri))
      return None

    if (!rangeOption.exists(r => r.contains(filePosition.offset)))
      return None

    val childResults = childElements.flatMap(child => child.getChildForPosition(filePosition))
    Some(childResults.headOption.getOrElse({
      this
    }))
  }
}
