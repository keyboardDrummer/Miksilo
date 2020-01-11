package jsonRpc

import com.dhpcs.jsonrpc.{JsonRpcNotificationMessage, JsonRpcRequestMessage, JsonRpcResponseMessage}

import scala.concurrent.Future

trait JsonRpcHandler {
  def dispose(): Unit
  def handleNotification(notification: JsonRpcNotificationMessage): Unit
  def handleRequest(request: JsonRpcRequestMessage): Future[JsonRpcResponseMessage]
}
