package jsonRpc

import java.io.OutputStream

/**
  * A class to write Json RPC messages on an output stream, following the Language Server Protocol.
  * It produces the following format:
  *
  * <Header> '\r\n' <Content>
  *
  * Header := FieldName ':' FieldValue '\r\n'
  *
  * Currently there are two defined header fields:
  * - 'Content-Length' in bytes (required)
  * - 'Content-Type' (string), defaults to 'application/vscode-jsonrpc; charset=utf8'
  *
  * @note The header part is defined to be ASCII encoded, while the content part is UTF8.
  */
class JVMMessageWriter(out: OutputStream) extends MessageWriter with LazyLogging {
  private val ContentLen = "Content-Length"

  /** Lock protecting the output stream, so multiple writes don't mix message chunks. */
  private val lock = new Object

  /**
   * Write a message to the output stream. This method can be called from multiple threads,
   * but it may block waiting for other threads to finish writing.
   */
  def write(str: String): Unit = lock.synchronized {
    val contentBytes = str.getBytes(MessageReader.Utf8Charset)
    val headers = Map(ContentLen -> contentBytes.length)
      .map { case (k, v) => s"$k: $v" }
      .mkString("", "\r\n", "\r\n\r\n")

    logger.debug(s"$headers\n\n$str")
    logger.debug(s"payload: $str")

    val headerBytes = headers.getBytes(MessageReader.AsciiCharset)

    lock.synchronized({
      out.write(headerBytes)
      out.write(contentBytes)
      out.flush()
    })
  }
}
