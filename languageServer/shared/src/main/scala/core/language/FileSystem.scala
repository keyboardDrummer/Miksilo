package core.language

import core.parsers.core.ParseText

trait TextChangeHandler {
  def handleChange(from: Int, until: Int, text: String): Unit
}

trait FileSystem {
  def getFileParseText(path: String): ParseText
  def getFile(path: String): String
  def setTextChangedHandler(uri: String, handler: TextChangeHandler): Unit
}

object EmptyFileSystem extends FileSystem {
  override def getFile(path: String): String = throw new IllegalArgumentException(s"no file for path $path")

  override def setTextChangedHandler(uri: String, handler: TextChangeHandler): Unit = {}

  override def getFileParseText(path: String) = throw new IllegalArgumentException(s"no file for path $path")
}

case class InMemoryFileSystem(files: Map[String, String]) extends FileSystem {
  override def getFile(path: String): String = files(path)

  override def setTextChangedHandler(uri: String, handler: TextChangeHandler): Unit = {}

  override def getFileParseText(path: String) = {
    val result = new ParseText()
    result.arrayOfChars = getFile(path).toCharArray
    result
  }
}