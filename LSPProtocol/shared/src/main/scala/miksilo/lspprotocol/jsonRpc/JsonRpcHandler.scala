package miksilo.lspprotocol.jsonRpc

import miksilo.lspprotocol.jsonRpc.{JsonRpcNotificationMessage, JsonRpcRequestMessage, JsonRpcResponseMessage}

import scala.concurrent.Future

trait JsonRpcHandler {
  def dispose(): Unit
  def handleNotification(notification: JsonRpcNotificationMessage): Unit
  def handleRequest(request: JsonRpcRequestMessage): Future[JsonRpcResponseMessage]
}
