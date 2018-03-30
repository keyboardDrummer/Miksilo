package languageServer.lsp

import com.dhpcs.jsonrpc.JsonRpcMessage.{ArrayParams, CorrelationId, ObjectParams, Params}
import com.dhpcs.jsonrpc._
import com.typesafe.scalalogging.LazyLogging
import play.api.libs.json._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.Promise

class SimpleJsonRpcHandler(connection: JsonRpcConnection) extends JsonRpcHandler with LazyLogging {
  connection.setHandler(this)

  private var requestHandlers: Map[String, JsonRpcRequestMessage => JsonRpcResponseMessage] = Map.empty
  private val notificationHandlers: mutable.Map[String, ListBuffer[JsonRpcNotificationMessage => Unit]] = mutable.Map.empty

  def sendNotification[Notification](method: String, notification: Notification)(implicit format: OFormat[Notification]): Unit = {
    val json = format.writes(notification)
    connection.sendNotification(new JsonRpcNotificationMessage(method, ObjectParams(json)))
  }

  def sendRequest[Request, Response](method: String, correlationId: CorrelationId, request: Request)
                                    (implicit requestFormat: OWrites[Request], responseFormat: Reads[Response]):
    Promise[Response] = {
    val requestJson = requestFormat.writes(request)
    val requestMessage = new JsonRpcRequestMessage(method, ObjectParams(requestJson), correlationId)

    val result = Promise[Response]
    connection.sendRequest(requestMessage, {
      case success: JsonRpcResponseSuccessMessage =>
        responseFormat.reads(success.result) match {
          case JsSuccess(response, _) => result.success(response)
          case JsError(errors) => result.failure(new Exception())
        }
      case error: JsonRpcResponseErrorMessage =>
        result.failure(new Exception())
    })
    result
  }

  override def handleNotification(notification: JsonRpcNotificationMessage): Unit = {
    notificationHandlers.get(notification.method) match {
      case Some(handlers) => handlers.foreach(handler => {
        try {
          handler(notification)
        } catch {
          case error: Throwable => logger.error("Notification handler failed with: " + error)
        }
      })
      case None =>
    }
  }

  override def handleRequest(request: JsonRpcRequestMessage): JsonRpcResponseMessage = {
    requestHandlers.get(request.method) match {
      case Some(handle) => handle(request)
      case None => JsonRpcResponseErrorMessage.methodNotFound(request.method, request.id)
    }
  }

  def toJson(params: Params): JsValue = params match {
    case obj: ObjectParams => obj.value
    case array: ArrayParams => array.value
    case _ => JsNull
  }

  def addNotificationHandler[Notification](method: String, handler: Notification => Unit)(notificationFormat: OFormat[Notification]): Unit = {
    val handlers = getNotificationHandlersForMethod(method)
    val handle = (notification: JsonRpcNotificationMessage) => {
      val requestJson = toJson(notification.params)
      notificationFormat.reads(requestJson) match {
        case JsSuccess(typedRequest, _) =>
          handler(typedRequest)
        case JsError(errors) =>
          logger.error("Failed to parse notification message")
      }
    }
    handlers.append(handle)
  }

  def getNotificationHandlersForMethod[Notification](method: String): ListBuffer[JsonRpcNotificationMessage => Unit] = {
    notificationHandlers.getOrElseUpdate(method, ListBuffer.empty)
  }

  def addRequestHandler[Request, Response](method: String, handler: Request => Response)
                                          (requestFormat: Reads[Request], responseFormat: Writes[Response]): Unit = {
    val handle = (request: JsonRpcRequestMessage) => {
      val requestJson = toJson(request.params)
      requestFormat.reads(requestJson) match {
        case JsSuccess(typedRequest, _) =>
          val typedResponse = handler(typedRequest)
          val jsonResponse = responseFormat.writes(typedResponse)
          JsonRpcResponseSuccessMessage(jsonResponse, request.id)
        case JsError(errors) =>
          JsonRpcResponseErrorMessage.invalidParams(null, request.id)
      }
    }
    requestHandlers += method -> handle
  }
}
