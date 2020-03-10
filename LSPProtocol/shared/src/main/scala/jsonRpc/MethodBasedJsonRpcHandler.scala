package jsonRpc

import java.io.{PrintWriter, StringWriter}

import com.dhpcs.jsonrpc.JsonRpcMessage.{ArrayParams, CorrelationId, ObjectParams, Params}
import com.dhpcs.jsonrpc._
import core.LazyLogging
import play.api.libs.json._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future}

object MethodBasedJsonRpcHandler {
  def toJson(params: Params): JsValue = params match {
    case obj: ObjectParams => obj.value
    case array: ArrayParams => array.value
    case _ => JsNull
  }
}

class MethodBasedJsonRpcHandler(connection: JsonRpcConnection) extends JsonRpcHandler with LazyLogging {

  private var requestHandlers: Map[String, JsonRpcRequestMessage => JsonRpcResponseMessage] = Map.empty
  private val notificationHandlers: mutable.Map[String, ListBuffer[JsonRpcNotificationMessage => Unit]] = new mutable.HashMap

  def sendNotification[Notification](method: String, notification: Notification)(implicit format: OFormat[Notification]): Unit = {
    val json = format.writes(notification)
    connection.sendNotification(new JsonRpcNotificationMessage(method, ObjectParams(json)))
  }

  def sendRequest[Request, Response](method: String, correlationId: CorrelationId, request: Request)
                                    (implicit requestFormat: OWrites[Request], responseFormat: Reads[Response]):
    Future[Response] = {

    val requestJson = requestFormat.writes(request)
    val requestMessage = new JsonRpcRequestMessage(method, ObjectParams(requestJson), correlationId)

    connection.sendRequest(requestMessage).flatMap({
      case success: JsonRpcResponseSuccessMessage =>
        responseFormat.reads(success.result) match {
          case JsSuccess(response, _) => Future.successful(response)
          case JsError(errors) => Future.failed(new Exception())
        }
      case error: JsonRpcResponseErrorMessage =>
        Future.failed(new Exception(error.message))
    })(ExecutionContext.global)
  }

  override def handleNotification(notification: JsonRpcNotificationMessage): Unit = {
    notificationHandlers.get(notification.method) match {
      case Some(handlers) =>
        handlers.foreach(handler => {
        try {
          handler(notification)
        } catch {
          case error: Throwable =>
            val stringWriter = new StringWriter
            error.printStackTrace(new PrintWriter(stringWriter))
            logger.error("Notification handler failed with: " + error +
            "\n Stack trace:" + stringWriter.toString)
        }
      })
      case None =>
    }
  }

  override def handleRequest(request: JsonRpcRequestMessage): Future[JsonRpcResponseMessage] = {
    Future.successful(requestHandlers.get(request.method) match {
      case Some(handle) => handle(request)
      case None => JsonRpcResponseErrorMessage.methodNotFound(request.method, request.id)
    })
  }

  def addNotificationHandler[Notification](method: String, handler: Notification => Unit)(notificationFormat: OFormat[Notification]): Unit = {
    val handlers = getNotificationHandlersForMethod(method)
    val handle = (notification: JsonRpcNotificationMessage) => {
      val requestJson = MethodBasedJsonRpcHandler.toJson(notification.params)
      notificationFormat.reads(requestJson) match {
        case JsSuccess(typedRequest, _) =>
          handler(typedRequest)
        case JsError(errors) =>
          logger.error(s"Failed to parse notification message for $method, got errors: $errors")
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
      val requestJson = MethodBasedJsonRpcHandler.toJson(request.params)
      requestFormat.reads(requestJson) match {
        case JsSuccess(typedRequest, _) =>
          val typedResponse = handler(typedRequest)
          val jsonResponse = responseFormat.writes(typedResponse)
          JsonRpcResponseSuccessMessage(jsonResponse, request.id)
        case error:JsError =>
          JsonRpcResponseErrorMessage.invalidParams(error, request.id)
      }
    }
    requestHandlers += method -> handle
  }

  override def dispose(): Unit = {}
}
