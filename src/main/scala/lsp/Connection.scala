package lsp

import com.typesafe.scalalogging.LazyLogging
import langserver.messages.Notification

import scala.collection.mutable.ListBuffer
import scala.util.Try

trait Connection extends LazyLogging {
  def showMessage(messageType: Int, message: String): Unit = {}
  def setServer(languageServer: LanguageServer)
  def start(): Unit = {}
  val notificationHandlers: ListBuffer[Notification => Unit] = ListBuffer.empty

  def notifySubscribers(n: Notification): Unit = {
    notificationHandlers.foreach(f =>
      Try(f(n)).recover { case e => logger.error("failed notification handler", e) })
  }
}
