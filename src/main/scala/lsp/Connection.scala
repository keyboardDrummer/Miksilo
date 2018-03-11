package lsp

import langserver.messages.Notification
import scala.collection.mutable.ListBuffer

trait Connection {
  def showMessage(messageType: Int, message: String): Unit = {}
  def setServer(languageServer: LanguageServer)
  def start(): Unit = {}
  def notificationHandlers: ListBuffer[Notification => Unit] = ListBuffer.empty
}
