package miksilo.languageServer.core.language

import miksilo.editorParser.parsers.core.ParseText

trait DocumentEventListener {
  def handleChange(from: Int, until: Int, text: String): Unit
  def handleClose(): Unit
}

trait FileSystem {
  def getFileParseText(path: String): ParseText
  def getFile(path: String): String
  def setDocumentEventListener(uri: String, handler: DocumentEventListener): Unit
}

object EmptyFileSystem extends FileSystem {
  override def getFile(path: String): String = throw new IllegalArgumentException(s"no file for path $path")

  override def setDocumentEventListener(uri: String, handler: DocumentEventListener): Unit = {}

  override def getFileParseText(path: String) = throw new IllegalArgumentException(s"no file for path $path")
}

case class InMemoryFileSystem(files: Map[String, String]) extends FileSystem {
  override def getFile(path: String): String = files(path)

  override def setDocumentEventListener(uri: String, handler: DocumentEventListener): Unit = {}

  override def getFileParseText(path: String) = {
    val result = new ParseText()
    result.arrayOfChars = getFile(path).toCharArray
    result
  }
}