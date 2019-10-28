package jsonRpc

import com.dhpcs.jsonrpc.{JsonRpcMessage, JsonRpcNotificationMessage, JsonRpcRequestMessage, JsonRpcResponseMessage}

import scala.concurrent.{Future, Promise}

trait WorkItem
case class Notification(notification: JsonRpcNotificationMessage) extends WorkItem
case class Request(request: JsonRpcRequestMessage, result: Promise[JsonRpcResponseMessage]) extends WorkItem

abstract class MessagePreprocessor(original: JsonRpcHandler) extends AsyncJsonRpcHandler {

  def aggregate(items: List[WorkItem]): List[WorkItem]

  var messages: List[WorkItem] = List.empty[WorkItem]
  val lock = new Object()

  @volatile
  private var streamClosed = false

  private class PumpInput extends Thread("Input Reader") {
    override def run() {
      while (!streamClosed) {
        popWorkItem() match {
          case None =>
          case Some(message) => message match {
            case Notification(notification) => original.handleNotification(notification)
            case Request(request, result) => result.success(original.handleRequest(request))
          }
        }
      }
    }
  }

  (new PumpInput).start()

  def dispose(): Unit = {
    streamClosed = true
  }

  def popWorkItem(): Option[WorkItem] = {
    lock.synchronized {
      messages match {
        case Nil =>
          if (!streamClosed) {
            lock.wait()
            popWorkItem()
          }
          else {
            None
          }
        case head :: tail =>
          messages = tail
          Some(head)
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
      messages = aggregate(message :: messages)
      lock.notify()
    }
  }

}
