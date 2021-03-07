package miksilo.lspprotocol.jsonRpc

import java.nio.charset.Charset

import miksilo.lspprotocol.jsonRpc.JsonRpcMessage.{CorrelationId, NoCorrelationId}
import miksilo.lspprotocol.jsonRpc._
import miksilo.editorParser.LazyLogging
import play.api.libs.json.{Format, JsError, Json}

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}
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

class JsonRpcConnection(reader: MessageReader, writer: MessageWriter) extends LazyLogging {

  private var responseHandlers: Map[CorrelationId, JsonRpcResponseMessage => Unit] = Map.empty
  protected var handler: JsonRpcHandler = _

  def parseJsonRpcMessage(message: String): JsonRpcMessage = {
    logger.debug(s"Received $message")
    Try(Json.parse(message)) match {
      case Failure(exception) =>
        JsonRpcResponseErrorMessage.parseError(exception, NoCorrelationId)

      case Success(json) =>
        Json.fromJson[JsonRpcMessage](json).fold({ errors =>
          JsonRpcResponseErrorMessage.invalidRequest(JsError(errors), NoCorrelationId)
        }, x => x)
    }
  }

  private var stopped = false
  def stop(): Unit = {
    stopped = true
  }

  def listen(): Unit = {
    def processItem(): Unit = {
      reader.nextPayload().foreach({
        case null =>
          handler.dispose()
        case jsonString =>
          if (stopped) {
            handler.dispose()
            return
          }
          handleMessage(jsonString)
          processItem()
      })
    }
    processItem()
  }

  private def handleMessage(message: String): Unit = {
    parseJsonRpcMessage(message) match {
      case error: JsonRpcResponseErrorMessage => write(error)
      case notification: JsonRpcNotificationMessage => handler.handleNotification(notification)

      case request: JsonRpcRequestMessage => handler.handleRequest(request).
        foreach(response => write(response))(ExecutionContext.global)

      case response: JsonRpcResponseMessage =>
        responseHandlers.get(response.id).fold({
          logger.info(s"Received a response for which there was no handler: $response")
        })(handler => handler(response))
      case _ => throw new Error(s"JSON-RPC message $message is not supported yet")
    }
  }

  def setHandler(handler: JsonRpcHandler): Unit = {
    this.handler = handler
  }

  def sendRequest(request: JsonRpcRequestMessage): Future[JsonRpcResponseMessage] = {
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