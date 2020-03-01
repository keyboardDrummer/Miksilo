package jsonRpc

import core.LazyLogging

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.scalajs.js
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * A Language Server message Reader. It expects the following format:
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
class NodeMessageReader(in: js.Dynamic) extends MessageReader with LazyLogging {

  val BufferSize = 8192

  private var data = ArrayBuffer.empty[Byte]
  private var streamClosed = false

  var dataArrival: Promise[Unit] = _

  def receiveData(): Unit = {
    if (dataArrival != null) {
      dataArrival.success(())
      dataArrival = null
    }
  }

  def waitForData(): Future[Unit] = {
    if (this.dataArrival != null)
      return this.dataArrival.future

    this.dataArrival = Promise[Unit]
    this.dataArrival.future
  }

  in.on("data", (chunk: js.Array[Byte]) => {
    data ++= chunk.toArray
    receiveData()
  })

  in.on("end", () => {
    streamClosed = true
    receiveData() // some threads might be still waiting for input
  })

  /**
   * Return headers, if any are available. It returns only full headers, after the
   * \r\n\r\n mark has been seen.
   *
   * @return A map of headers. If the map is empty it could be that the input stream
   *         was closed, or there were no headers before the delimiter. You can disambiguate
   *         by checking {{{this.streamClosed}}}
   */
  final def readHeaders(): Future[Map[String, String]] = {
    val EmptyPair = "" -> ""
    val EmptyMap = Map.empty[String, String]

    def atDelimiter(idx: Int): Boolean = {
      (data.size >= idx + 4
        && data(idx) == '\r'
        && data(idx + 1) == '\n'
        && data(idx + 2) == '\r'
        && data(idx + 3) == '\n')
    }

    if (data.size < 4 && !streamClosed)
      return waitForData().flatMap(_ => readHeaders())(ExecutionContext.global)

    if (streamClosed) return Future.successful(EmptyMap)

    var i = 0
    while (i + 4 < data.size && !atDelimiter(i)) {
      i += 1
    }

    if (atDelimiter(i)) {
      val headers = new String(data.slice(0, i).toArray, MessageReader.AsciiCharset)

      val pairs = headers.split("\r\n").filter(_.trim.length() > 0) map { line =>
        line.split(":") match {
          case Array(key, value) => key.trim -> value.trim
          case _ =>
            EmptyPair
        }
      }

      // drop headers
      data = data.drop(i + 4)

      // if there was a malformed header we keep trying to re-sync and read again
      if (pairs.contains(EmptyPair)) {
        readHeaders()
      } else Future.successful(pairs.toMap)
    } else if (streamClosed) {
      Future.successful(EmptyMap)
    } else {
      waitForData().flatMap(_ => readHeaders())
    }
  }

  /**
   * Return `len` bytes of content as a string encoded in UTF8.
   *
   * @note If the stream was closed this method returns the empty string.
   */
  def getContent(len: Int): Future[String] = {
    if (data.size < len && !streamClosed)
      return waitForData().flatMap(_ => getContent(len))

    if (streamClosed) Future.successful("")
    else {
      assert(data.size >= len)
      val content = data.take(len).toArray
      data = data.drop(len)
      Future.successful(new String(content, MessageReader.Utf8Charset))
    }
  }

  /**
   * Return the next JSON RPC content payload. Blocks until enough data has been received.
   */
  def nextPayload(): Future[String] = {
    if (streamClosed) Future.successful(null)
    else {
      // blocks until headers are available
      readHeaders().flatMap(headers => {
        if (headers.isEmpty && streamClosed)
          Future.successful(null)
        else {
          val length = headers.get("Content-Length") match {
            case Some(len) => try len.toInt catch {
              case _: NumberFormatException => -1
            }
            case _ => -1
          }

          if (length > 0) {
            getContent(length).map(content => if (content.isEmpty && streamClosed) null else content)
          } else {
            nextPayload()
          }
        }
      })
    }
  }
}