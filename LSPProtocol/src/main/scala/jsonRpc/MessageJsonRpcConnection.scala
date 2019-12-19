package jsonRpc

import java.nio.charset.Charset

import com.dhpcs.jsonrpc.JsonRpcMessage.CorrelationId
import com.dhpcs.jsonrpc.{JsonRpcNotificationMessage, JsonRpcRequestMessage, JsonRpcResponseErrorMessage, JsonRpcResponseMessage}
import play.api.libs.json.{Format, Json}

import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global

trait MessageReader {
  def nextPayload(): Future[String]
}

object MessageReader {
  val AsciiCharset = Charset.forName("ASCII")
  val Utf8Charset = Charset.forName("UTF-8")
}

trait MessageWriter {
  def write(msg: String): Unit
}

class MessageJsonRpcConnection(reader: MessageReader, writer: MessageWriter) extends JsonRpcConnection {

  private var responseHandlers: Map[CorrelationId, JsonRpcResponseMessage => Unit] = Map.empty

  override def listen(): Unit = {

    def processItem(): Unit = {
      reader.nextPayload().foreach {
        case null =>
          handler.dispose()
        case jsonString =>
          handleMessage(jsonString)
          processItem()
      }
    }
    processItem()
  }

  private def handleMessage(message: String): Unit = {
    parseJsonRpcMessage(message) match {
      case error: JsonRpcResponseErrorMessage => write(error)
      case notification: JsonRpcNotificationMessage => handler.handleNotification(notification)

      case request: JsonRpcRequestMessage => handler.handleRequest(request).
        foreach(response => write(response))

      case response: JsonRpcResponseMessage =>
        responseHandlers.get(response.id).fold({
          logger.info(s"Received a response for which there was no handler: $response")
        })(handler => handler(response))
      case _ => throw new Error(s"JSON-RPC message $message is not supported yet")
    }
  }

  override def setHandler(handler: JsonRpcHandler): Unit = {
    this.handler = handler
  }

  override def sendRequest(request: JsonRpcRequestMessage): Future[JsonRpcResponseMessage] = {
    val result = Promise[JsonRpcResponseMessage]

    // TODO decide after what time to remove responseHandlers
    responseHandlers += request.id -> ((response: JsonRpcResponseMessage) => result.success(response))

    write(request)
    result.future
  }

  def sendNotification(notification: JsonRpcNotificationMessage): Unit = {
    write(notification)
  }

  private def write[T](msg: T)(implicit o: Format[T]): Unit = {
    val str = Json.stringify(o.writes(msg))
    writer.write(str)
  }
}