package miksilo.lspprotocol.jsonRpc

import miksilo.editorParser.LazyLogging

import scala.scalajs.js

class NodeMessageWriter(out: js.Dynamic) extends MessageWriter with LazyLogging {
  private val ContentLen = "Content-Length"

  def write(str: String): Unit = {

    //require(h.get(ContentLen).isEmpty)

    val contentBytes = str.getBytes(MessageReader.Utf8Charset)
    val headers = Map(ContentLen -> contentBytes.length)
      .map { case (k, v) => s"$k: $v" }
      .mkString("", "\r\n", "\r\n\r\n")

    logger.debug(s"$headers\n\n$str")
    logger.debug(s"payload: $str")

    val headerBytes = headers.getBytes(MessageReader.AsciiCharset)

    out.write(headers) //headerBytes.toJSArray)
    out.write(str) //contentBytes.toJSArray)
    //out.flush()
  }
}
