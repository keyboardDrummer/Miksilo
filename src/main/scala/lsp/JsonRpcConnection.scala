package lsp

import java.io.{InputStream, OutputStream}
import java.util.concurrent.Executors

import com.dhpcs.jsonrpc.JsonRpcMessage._
import com.dhpcs.jsonrpc._
import langserver.core.{MessageReader, MessageWriter}
import langserver.types.Diagnostic
import play.api.libs.json._

import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService, Future}
import scala.util.{Failure, Success, Try}

/**
  * A connection that reads and writes Language Server Protocol messages.
  *
  * @note Commands are executed asynchronously via a thread pool
  * @note Notifications are executed synchronously on the calling thread
  * @note The command handler returns Any because sometimes response objects can't be part
  *       of a sealed hierarchy. For instance, goto definition returns a {{{Seq[Location]}}}
  *       and that can't subclass anything other than Any
  */
class JsonRpcConnection(inStream: InputStream, outStream: OutputStream)
  extends Connection {

  var server: LanguageServer = _
  override def setServer(languageServer: LanguageServer): Unit = {
    server = languageServer
  }

  private val msgReader = new MessageReader(inStream)
  private val msgWriter = new MessageWriter(outStream)

  // 4 threads should be enough for everyone
  implicit private val commandExecutionContext: ExecutionContextExecutorService = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))

  def sendNotification(params: Notification): Unit = {
    val json = Notification.write(params)
    msgWriter.write(json)
  }

  /**
    * A notification sent to the client to show a message.
    *
    * @param tpe One of MessageType values
    * @param message The message to display in the client
    */
  override def showMessage(tpe: Int, message: String): Unit = {
    sendNotification(ShowMessageParams(tpe, message))
  }

  /**
    * The log message notification is sent from the server to the client to ask
    * the client to log a particular message.
    *
    * @param tpe One of MessageType values
    * @param message The message to display in the client
    */
  def logMessage(tpe: Int, message: String): Unit = {
    sendNotification(LogMessageParams(tpe, message))
  }

  /**
    * Publish compilation errors for the given file.
    */
  def publishDiagnostics(uri: String, diagnostics: Seq[Diagnostic]): Unit = {
    sendNotification(PublishDiagnostics(uri, diagnostics))
  }

  def start() {
    var streamClosed = false
    do {
      msgReader.nextPayload() match {
        case None => streamClosed = true

        case Some(jsonString) =>
          readJsonRpcMessage(jsonString) match {
            case Left(e) =>
              msgWriter.write(e)

            case Right(message) => message match {
              case notification: JsonRpcNotificationMessage =>
                Option(Notification.read(notification)).fold(
                  logger.error(s"No notification type exists with method=${notification.method}")
                )(_.fold(
                  errors => logger.error(s"Invalid Notification: $errors - Message: $message"),
                  notifySubscribers))

              case request: JsonRpcRequestMessage =>
                unpackRequest(request) match {
                  case (_, Left(e)) => msgWriter.write(e)
                  case (None, Right(c)) => // this is disallowed by the language server specification
                    logger.error(s"Received request without 'id'. $c")
                  case (Some(id), Right(command)) => handleCommand(request.method, id, command)
                }

              case response: JsonRpcResponseMessage =>
                logger.info(s"Received response: $response")

              case m =>
                logger.error(s"Received unknown message: $m")
            }
            case m => logger.error(s"Received unknown message: $m")
          }
      }
    } while (!streamClosed)
  }

  private def readJsonRpcMessage(jsonString: String): Either[JsonRpcResponseErrorMessage, JsonRpcMessage] = {
    logger.debug(s"Received $jsonString")
    Try(Json.parse(jsonString)) match {
      case Failure(exception) =>
        Left(JsonRpcResponseErrorMessage.parseError(exception,NoCorrelationId))

      case Success(json) =>
        Json.fromJson[JsonRpcMessage](json).fold({ errors =>
          Left(JsonRpcResponseErrorMessage.invalidRequest(JsError(errors),NoCorrelationId))
        }, Right(_))
    }
  }

  private def readCommand(jsonString: String): (Option[CorrelationId], Either[JsonRpcResponseErrorMessage, ServerCommand]) =
    Try(Json.parse(jsonString)) match {
      case Failure(exception) =>
        None -> Left(JsonRpcResponseErrorMessage.parseError(exception,NoCorrelationId ))

      case Success(json) =>
        Json.fromJson[JsonRpcRequestMessage](json).fold(
          errors => None -> Left(JsonRpcResponseErrorMessage.invalidRequest(JsError(errors),NoCorrelationId)),

          jsonRpcRequestMessage =>
            Option(ServerCommand.read(jsonRpcRequestMessage))
              .fold[(Option[CorrelationId], Either[JsonRpcResponseErrorMessage, ServerCommand])](
              Some(jsonRpcRequestMessage.id) -> Left(JsonRpcResponseErrorMessage.methodNotFound(jsonRpcRequestMessage.method,jsonRpcRequestMessage.id )))(commandJsResult => commandJsResult.fold(
              errors => Some(jsonRpcRequestMessage.id) -> Left(JsonRpcResponseErrorMessage.invalidParams(JsError(errors),jsonRpcRequestMessage.id)),
              command => Some(jsonRpcRequestMessage.id) -> Right(command))))

    }

  private def unpackRequest(request: JsonRpcRequestMessage): (Option[CorrelationId], Either[JsonRpcResponseErrorMessage, ServerCommand]) = {
    Option(ServerCommand.read(request))
      .fold[(Option[CorrelationId], Either[JsonRpcResponseErrorMessage, ServerCommand])](
      Some(request.id) -> Left(JsonRpcResponseErrorMessage.methodNotFound(request.method,request.id )))(
      commandJsResult => commandJsResult.fold(errors =>
        Some(request.id) -> Left(JsonRpcResponseErrorMessage.invalidParams(JsError(errors),request.id )),
        command => Some(request.id) -> Right(command)))

  }

  private def handleCommand(method: String, id: CorrelationId, command: ServerCommand) = {
    Future(handle(method, command)).map { result =>
      val t = Try{ResultResponse.write(result, id)}
      t.recover{case e => logger.error("ResultResponse.write:"+result); e.printStackTrace }
      t.foreach{rJson => msgWriter.write(rJson)}
    }
  }

  private def handle(method: String, command: ServerCommand): Any = {
    (method, command) match {
      case (_, InitializeParams(pid, rootPath, capabilities)) =>
        InitializeResult(server.initialize(pid, rootPath, capabilities))
      case ("textDocument/completion", TextDocumentCompletionRequest(TextDocumentPositionParams(textDocument, position))) =>
        server.asInstanceOf[CompletionProvider].completionRequest(textDocument, position)
      case ("textDocument/definition", TextDocumentDefinitionRequest(TextDocumentPositionParams(textDocument, position))) =>
        server.asInstanceOf[GotoProvider].gotoDefinitionRequest(textDocument, position)
      case ("textDocument/hover", TextDocumentHoverRequest(TextDocumentPositionParams(textDocument, position))) =>
        server.asInstanceOf[HoverProvider].hoverRequest(textDocument, position)
      case ("textDocument/documentSymbol", DocumentSymbolParams(tdi)) =>
        DocumentSymbolResult(server.asInstanceOf[DocumentSymbolProvider].documentSymbols(tdi))

      case (_, Shutdown()) =>
        server.shutdown()
        ShutdownResult(0) // the value is a dummy, because Play Json needs to serialize something
      case c =>
        server.logger.error(s"Unknown command $c")
        sys.error("Unknown command")
    }
  }
}
