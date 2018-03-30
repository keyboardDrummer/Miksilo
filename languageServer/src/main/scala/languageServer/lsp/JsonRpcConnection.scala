package languageServer.lsp

import java.io.{InputStream, OutputStream}
import java.util.concurrent.Executors

import com.dhpcs.jsonrpc.JsonRpcMessage._
import com.dhpcs.jsonrpc._
import com.typesafe.scalalogging.LazyLogging
import langserver.core.{MessageReader, MessageWriter}
import play.api.libs.json._

import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}
import scala.util.{Failure, Success, Try}

trait JsonRpcHandler {
  def handleNotification(notification: JsonRpcNotificationMessage)
  def handleRequest(request: JsonRpcRequestMessage): JsonRpcResponseMessage
}

/**
  * A connection that reads and writes Language Server Protocol messages.
  *
  * @note Commands are executed asynchronously via a thread pool
  * @note Notifications are executed synchronously on the calling thread
  * @note The command handler returns Any because sometimes response objects can't be part
  *       of a sealed hierarchy. For instance, goto definition returns a {{{Seq[Location]}}}
  *       and that can't subclass anything other than Any
  */
class JsonRpcConnection(inStream: InputStream, outStream: OutputStream)
  extends LazyLogging {

  private val msgReader = new MessageReader(inStream)
  private val msgWriter = new MessageWriter(outStream)
  private var handler: JsonRpcHandler = _

  def setHandler(handler: JsonRpcHandler): Unit = {
    this.handler = handler
  }

  def sendNotification(notification: JsonRpcNotificationMessage): Unit = {
    msgWriter.write(notification)
  }

  def sendRequest(request: JsonRpcRequestMessage, responseHandler: JsonRpcResponseMessage => Unit): Unit = {
    responseHandlers += request.id -> responseHandler //TODO add a timeout for cleaning up ResponseHandlers
    msgWriter.write(request)
  }

  private var responseHandlers: Map[CorrelationId, JsonRpcResponseMessage => Unit] = Map.empty

  def listen() {
    if (handler == null)
      throw new Exception("Handler must first be set")

    var streamClosed = false
    while (!streamClosed) {
      msgReader.nextPayload() match {
        case None => streamClosed = true
        case Some(jsonString) => handleMessage(jsonString)
      }
    }
  }

  private def handleMessage(message: String): Unit = {
    parseJsonRpcMessage(message) match {
      case error: JsonRpcResponseErrorMessage => msgWriter.write(error)
      case notification: JsonRpcNotificationMessage => handler.handleNotification(notification)

      case request: JsonRpcRequestMessage => msgWriter.write(handler.handleRequest(request))

      case response: JsonRpcResponseMessage =>
        responseHandlers.get(response.id).fold({
          logger.info(s"Received a response for which there was no handler: $response")
        })(handler => handler(response))
      case _ => ???
    }
  }

  private def parseJsonRpcMessage(message: String): JsonRpcMessage = {
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

  // 4 threads should be enough for everyone
  implicit private val commandExecutionContext: ExecutionContextExecutorService = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))
}
