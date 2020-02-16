package util

import java.io._
import java.nio.charset.StandardCharsets

object StreamUtils {
  def streamToString(stream: InputStream): String = scala.io.Source.fromInputStream(stream).mkString
  def stringToStream(input: String) = new ByteArrayInputStream(input.getBytes(StandardCharsets.UTF_8))
}