package lsp

import com.dhpcs.jsonrpc.JsonRpcMessage.ObjectParams
import com.dhpcs.jsonrpc.JsonRpcNotificationMessage
import jsonRpc._
import play.api.libs.json.Json

class LSPServerMessagePreprocessor(original: AsyncJsonRpcHandler) extends MessagePreprocessor(original) {

  val changeParamsFormat = Json.format[DidChangeTextDocumentParams]
  override def aggregate(messages: CircularArrayBuffer[WorkItem]): Unit = {
    if (messages.size < 2) {
      return
    }

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
            val mergedChange = DidChangeTextDocumentParams(secondChange.textDocument, firstChange.contentChanges ++ secondChange.contentChanges)
            val newMessage = Notification(JsonRpcNotificationMessage(LSPProtocol.didChange, ObjectParams(changeParamsFormat.writes(mergedChange))))
            messages.remove(messages.size - 2, 2)
            messages += newMessage
          }
        }
      case _ =>
    }
  }
}
