package jsonRpc

import com.dhpcs.jsonrpc.{JsonRpcNotificationMessage, JsonRpcRequestMessage, JsonRpcResponseMessage}

import scala.concurrent.{ExecutionContext, Future, Promise}

trait WorkItem
case class Notification(notification: JsonRpcNotificationMessage) extends WorkItem
case class Request(request: JsonRpcRequestMessage, result: Promise[JsonRpcResponseMessage]) extends WorkItem

abstract class MessagePreprocessor(original: AsyncJsonRpcHandler) extends AsyncJsonRpcHandler {

  def aggregate(items: CircularArrayBuffer[WorkItem]): Unit

  var messages: CircularArrayBuffer[WorkItem] = new CircularArrayBuffer[WorkItem]
  val lock = new Object()

  @volatile
  private var streamClosed = false

  private class PumpInput extends Thread("Input Reader") {
    override def run() {
      while (!streamClosed) {
        dequeueWorkItem() match {
          case None =>
          case Some(message) => message match {
            case Notification(notification) => original.handleNotification(notification)
            case Request(request, resultPromise) => original.handleRequest(request).map(result => resultPromise.success(result))(ExecutionContext.global)
          }
        }
      }
    }
  }

  (new PumpInput).start()

  def dispose(): Unit = {
    streamClosed = true
  }

  def dequeueWorkItem(): Option[WorkItem] = {
    lock.synchronized {
      if (messages.isEmpty) {
        if (!streamClosed) {
          lock.wait()
          dequeueWorkItem()
        }
        else {
          None
        }
      }
      else {
        val message = messages.popLeft()
        Some(message)
      }
    }
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
    lock.synchronized {
      messages += message
      aggregate(messages)
      lock.notify()
    }
  }

}
