package jsonRpc

import com.dhpcs.jsonrpc.{JsonRpcNotificationMessage, JsonRpcRequestMessage, JsonRpcResponseMessage}

import scala.concurrent.{ExecutionContext, Future, Promise}

trait WorkItem
case class Notification(notification: JsonRpcNotificationMessage) extends WorkItem
case class Request(request: JsonRpcRequestMessage, result: Promise[JsonRpcResponseMessage]) extends WorkItem

abstract class MessagePreprocessor(original: JsonRpcHandler) extends JsonRpcHandler {

  def aggregate(items: CircularArrayBuffer[WorkItem]): Unit

  var messages: CircularArrayBuffer[WorkItem] = new CircularArrayBuffer[WorkItem]

  def dispose(): Unit = {}

  def dequeueWorkItem(): Unit = {
    if (messages.isEmpty)
      return

    val message = messages.popLeft()
    message match {
      case Notification(notification) => original.handleNotification(notification)
      case Request(request, resultPromise) => original.handleRequest(request).map(result => resultPromise.success(result))(ExecutionContext.global)
    }
    dequeueWorkItem()
  }

  override def handleNotification(notification: JsonRpcNotificationMessage): Unit = {
    addMessage(Notification(notification))
  }

  override def handleRequest(request: JsonRpcRequestMessage): Future[JsonRpcResponseMessage] = {
    val promise = Promise[JsonRpcResponseMessage]
    addMessage(Request(request, promise))
    promise.future
  }

  def addMessage(message: WorkItem): Unit = {
    messages += message
    aggregate(messages)
    ExecutionContext.global.execute(() => dequeueWorkItem())
  }

}
