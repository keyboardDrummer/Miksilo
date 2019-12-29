package core.language

import java.io.InputStream

trait FileSystem {
  def getFile(path: String): String
}

object EmptyFileSystem extends FileSystem {
  override def getFile(path: String): String = throw new IllegalArgumentException(s"no file for path $path")
}

case class InMemoryFileSystem(files: Map[String, String]) extends FileSystem {
  override def getFile(path: String): String = files(path)
}