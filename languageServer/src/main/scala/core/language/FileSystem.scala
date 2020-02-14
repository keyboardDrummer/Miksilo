package core.language

import core.parsers.editorParsers.SourceRange

trait TextChangeHandler {
  def handleChange(from: Int, until: Int, text: String): Unit
}

trait FileSystem {
  def getFile(path: String): String
  def setTextChangedHandler(uri: String, handler: TextChangeHandler)
}

object EmptyFileSystem extends FileSystem {
  override def getFile(path: String): String = throw new IllegalArgumentException(s"no file for path $path")

  override def setTextChangedHandler(uri: String, handler: TextChangeHandler): Unit = {}
}

case class InMemoryFileSystem(files: Map[String, String]) extends FileSystem {
  override def getFile(path: String): String = files(path)

  override def setTextChangedHandler(uri: String, handler: TextChangeHandler): Unit = {}
}