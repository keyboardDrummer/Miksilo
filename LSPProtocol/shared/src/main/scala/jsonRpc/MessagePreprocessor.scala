package jsonRpc

import com.dhpcs.jsonrpc.{JsonRpcNotificationMessage, JsonRpcRequestMessage, JsonRpcResponseMessage}

import scala.concurrent.{ExecutionContext, Future, Promise}

trait WorkItem {
  def method: String
}
case class Notification(notification: JsonRpcNotificationMessage) extends WorkItem {
  override def method: String = notification.method
}
case class Request(request: JsonRpcRequestMessage, result: Promise[JsonRpcResponseMessage]) extends WorkItem {
  override def method: String = request.method
}

/**
  * A asynchronous queue that executes work serially
  */
trait SerialWorkQueue[Item] {
  protected var handler: Item => Unit = _ => throw new Exception("Handler not set!")

  def setHandler(handler: Item => Unit): Unit = {
    this.handler = handler
  }

  def modifyQueue(queue: CircularArrayBuffer[Item] => Unit): Unit
}

abstract class MessagePreprocessor(original: JsonRpcHandler, workQueue: SerialWorkQueue[WorkItem]) extends JsonRpcHandler {

  workQueue.setHandler {
    case Notification(notification) => original.handleNotification(notification)
    case Request(request, resultPromise) => original.handleRequest(request).map(result => resultPromise.success(result))(ExecutionContext.global)
  }

  def aggregate(items: CircularArrayBuffer[WorkItem]): Unit

  def dispose(): Unit = {}

  override def handleNotification(notification: JsonRpcNotificationMessage): Unit = {
    addMessage(Notification(notification))
  }

  override def handleRequest(request: JsonRpcRequestMessage): Future[JsonRpcResponseMessage] = {
    val promise = Promise[JsonRpcResponseMessage]
    addMessage(Request(request, promise))
    promise.future
  }

  def addMessage(message: WorkItem): Unit = {
    workQueue.modifyQueue(messages => {
      messages.append(message)
      aggregate(messages)
    })
  }

}
