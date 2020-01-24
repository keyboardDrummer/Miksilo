package jsonRpc

import com.dhpcs.jsonrpc.JsonRpcMessage.ObjectParams
import com.dhpcs.jsonrpc.JsonRpcNotificationMessage
import lsp.{DidChangeTextDocumentParams, LSPProtocol, LSPServerMessagePreprocessor, TextDocumentIdentifier, VersionedTextDocumentIdentifier}
import org.scalatest.Assertion
import org.scalatest.funspec.AsyncFunSpec
import org.scalatest.funsuite.AnyFunSuite
import play.api.libs.json.Json

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.scalajs.js.Dynamic.{global => g}

class NodeMessageAggregation extends AsyncFunSpec {

  LazyLogging.logger = ConsoleLogger
  AfterIOExecution.context = SetTimeoutContext

  // Required because of https://github.com/scalatest/scalatest/issues/1039
  implicit override def executionContext: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global

  it("aggregates change messages") {
    var payloadCounter = 4
    var counter = 0
    import com.dhpcs.jsonrpc._
    val json = Json.format[DidChangeTextDocumentParams].writes(new DidChangeTextDocumentParams(new VersionedTextDocumentIdentifier("someUri", 0), Seq.empty))
    val msg = Json.stringify(JsonRpcNotificationMessage.JsonRpcNotificationMessageFormat.writes(new JsonRpcNotificationMessage(LSPProtocol.didChange, ObjectParams(json))))
    val result = Promise[Assertion]()
    val connection = new JsonRpcConnection(new MessageReader {
      override def nextPayload(): Future[String] = {
        if (payloadCounter == 0) {
          return Future.successful(null)
        }

        payloadCounter -= 1
        Future.successful(msg)
      }
    }, new MessageWriter {
      override def write(msg: String): Unit = { g.console.log(msg) }
    })
    val handler = new MethodBasedJsonRpcHandler(connection)
    connection.setHandler(new LSPServerMessagePreprocessor(handler))
    handler.addNotificationHandler[DidChangeTextDocumentParams](LSPProtocol.didChange, (notification: DidChangeTextDocumentParams) => {
      counter += 1
      if (payloadCounter == 0) {
        result.success(assertResult(1)(counter))
      }
    })(Json.format)
    connection.listen()
    result.future
  }
}
