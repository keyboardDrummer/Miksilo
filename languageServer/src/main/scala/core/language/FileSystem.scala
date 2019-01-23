package core.language

import scala.tools.nsc.interpreter.InputStream

trait FileSystem {
  def getFile(path: String): InputStream
}

object EmptyFileSystem extends FileSystem {
  override def getFile(path: String): InputStream = throw new IllegalArgumentException(s"no file for path $path")
}

case class InMemoryFileSystem(files: Map[String, InputStream]) extends FileSystem {
  override def getFile(path: String): InputStream = files(path)
}