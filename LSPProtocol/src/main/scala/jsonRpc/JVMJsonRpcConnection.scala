package jsonRpc

import java.io.{InputStream, OutputStream}
import java.util.concurrent.Executors

import com.dhpcs.jsonrpc.JsonRpcMessage.{CorrelationId, NoCorrelationId}
import com.dhpcs.jsonrpc._
import play.api.libs.json.{JsError, Json}

import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService, Future, Promise}
import scala.util.{Failure, Success, Try}

/**
  * A connection that reads and writes Language Server Protocol messages.
  *
  * @note Commands are executed asynchronously via a thread pool
  * @note Notifications are executed synchronously on the calling thread
  * @note The command handler returns Any because sometimes response objects can't be part
  *       of a sealed hierarchy. For instance, goto definition returns a {{{Seq[Location]}}}
  *       and that can't subclass anything other than Any
  */
class JVMJsonRpcConnection(inStream: InputStream, outStream: OutputStream)
  extends JsonRpcConnection with LazyLogging {

  private val msgReader = new MessageReader(inStream)
  private val msgWriter = new MessageWriter(outStream)
  private var handler: AsyncJsonRpcHandler = _
  private val writeLock = new Object()

  def setHandler(partner: AsyncJsonRpcHandler): Unit = {
    this.handler = partner
  }

  def sendNotification(notification: JsonRpcNotificationMessage): Unit = {
    msgWriter.write(notification)
  }

  private var responseHandlers: Map[CorrelationId, JsonRpcResponseMessage => Unit] = Map.empty

  def sendRequest(request: JsonRpcRequestMessage): Future[JsonRpcResponseMessage] = {
    val result = Promise[JsonRpcResponseMessage]

    // TODO decide after what time to remove responseHandlers
    responseHandlers += request.id -> ((response: JsonRpcResponseMessage) => result.success(response))

    msgWriter.write(request)
    result.future
  }

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
    handler.dispose()
  }

  private def handleMessage(message: String): Unit = {
    parseJsonRpcMessage(message) match {
      case error: JsonRpcResponseErrorMessage => msgWriter.write(error)
      case notification: JsonRpcNotificationMessage => handler.handleNotification(notification)

      case request: JsonRpcRequestMessage => handler.handleRequest(request).
        foreach(response => writeLock.synchronized(msgWriter.write(response)))

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
