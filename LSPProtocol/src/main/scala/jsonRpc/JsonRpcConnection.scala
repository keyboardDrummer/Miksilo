package jsonRpc

import com.dhpcs.jsonrpc._

import scala.concurrent.Future

trait Logger {
  def debug(message: String): Unit
  def info(message: String): Unit
  def error(message: String): Unit
}

trait LazyLogging {
  def logger: Logger = ???
}

trait AsyncJsonRpcHandler {
  def dispose(): Unit
  def handleNotification(notification: JsonRpcNotificationMessage)
  def handleRequest(request: JsonRpcRequestMessage): Future[JsonRpcResponseMessage]
}


trait JsonRpcHandler {
  def handleNotification(notification: JsonRpcNotificationMessage)
  def handleRequest(request: JsonRpcRequestMessage): JsonRpcResponseMessage
}

trait JsonRpcConnection {
  def listen(): Unit

  def setHandler(simpleConnection: AsyncJsonRpcHandler): Unit

  def sendRequest(requestMessage: JsonRpcRequestMessage): Future[JsonRpcResponseMessage]

  def sendNotification(message: JsonRpcNotificationMessage): Unit

}