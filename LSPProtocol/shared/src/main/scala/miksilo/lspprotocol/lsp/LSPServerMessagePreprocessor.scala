package miksilo.lspprotocol.lsp

import miksilo.lspprotocol.jsonRpc.JsonRpcMessage.ObjectParams
import miksilo.lspprotocol.jsonRpc.JsonRpcNotificationMessage
import miksilo.editorParser.LazyLogging
import miksilo.lspprotocol.jsonRpc.{CircularArrayBuffer, JsonRpcHandler, MessagePreprocessor, MethodBasedJsonRpcHandler, Notification, SerialWorkQueue, WorkItem}
import play.api.libs.json.Json

class LSPServerMessagePreprocessor(original: JsonRpcHandler, workQueue: SerialWorkQueue[WorkItem])
  extends MessagePreprocessor(original, workQueue) with LazyLogging {

  val changeParamsFormat = Json.format[DidChangeTextDocumentParams]
  override def aggregate(messages: CircularArrayBuffer[WorkItem]): Unit = {
    if (messages.size < 2) {
      return
    }
    logger.info(s"Aggregating ${messages.size} messages: ${messages.map(m => m.method).mkString(", ")}")

    val firstSecond = messages.takeRight(2)
    val first = firstSecond.head
    val second = firstSecond(1)
    (first, second) match {
      case (
        Notification(JsonRpcNotificationMessage(LSPProtocol.didChange, firstParams)),
        Notification(JsonRpcNotificationMessage(LSPProtocol.didChange, secondParams))
        ) =>
        for {
          firstChange <- changeParamsFormat.reads(MethodBasedJsonRpcHandler.toJson(firstParams))
          secondChange <- changeParamsFormat.reads(MethodBasedJsonRpcHandler.toJson(secondParams))
        } yield {
          if (firstChange.textDocument.uri == secondChange.textDocument.uri) {
            logger.info("Merging change messages")
            val mergedChange = DidChangeTextDocumentParams(secondChange.textDocument, firstChange.contentChanges ++ secondChange.contentChanges)
            val newMessage = Notification(JsonRpcNotificationMessage(LSPProtocol.didChange, ObjectParams(changeParamsFormat.writes(mergedChange))))
            messages.remove(messages.size - 2, 2)
            messages.append(newMessage)
          }
        }
      case _ =>
    }
  }
}
