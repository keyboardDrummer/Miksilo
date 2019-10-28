package lsp

import com.dhpcs.jsonrpc.JsonRpcMessage.ObjectParams
import com.dhpcs.jsonrpc.JsonRpcNotificationMessage
import jsonRpc._
import play.api.libs.json.{JsResult, Json}

class LSPServerAggregator(original: JsonRpcHandler) extends WorkAggregator(original) {

  val changeParamsFormat = Json.format[DidChangeTextDocumentParams]
  override def aggregate(messages: List[WorkItem]): List[WorkItem] = {

    messages match {
      case Notification(JsonRpcNotificationMessage(LSPProtocol.didChange, firstParams)) ::
        Notification(JsonRpcNotificationMessage(LSPProtocol.didChange, secondParams)) :: rest =>
        val maybeMessages: JsResult[List[WorkItem]] = for {
          firstChange <- changeParamsFormat.reads(SimpleJsonRpcHandler.toJson(firstParams))
          secondChange <- changeParamsFormat.reads(SimpleJsonRpcHandler.toJson(secondParams))
        } yield {
          if (firstChange.textDocument.uri == secondChange.textDocument.uri) {
            val mergedChange = DidChangeTextDocumentParams(firstChange.textDocument, firstChange.contentChanges ++ secondChange.contentChanges)
            val newMessage = Notification(JsonRpcNotificationMessage(LSPProtocol.didChange, ObjectParams(changeParamsFormat.writes(mergedChange))))
            newMessage :: rest
          } else {
            messages
          }
        }
        maybeMessages.getOrElse(messages)
      case _ => messages
    }
  }
}
