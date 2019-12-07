package jsonRpc

import com.dhpcs.jsonrpc.JsonRpcMessage.NoCorrelationId
import com.dhpcs.jsonrpc._
import play.api.libs.json.{JsError, Json}

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

trait JsonRpcConnection extends LazyLogging {
  protected var handler: JsonRpcHandler = _

  def listen(): Unit

  def sendRequest(requestMessage: JsonRpcRequestMessage): Future[JsonRpcResponseMessage]

  def sendNotification(message: JsonRpcNotificationMessage): Unit

  def setHandler(handler: JsonRpcHandler): Unit = {
    this.handler = handler
  }

  protected def parseJsonRpcMessage(message: String): JsonRpcMessage = {
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
}