package miksilo.lspprotocol.jsonRpc

import miksilo.lspprotocol.jsonRpc.JsonRpcMessage.ParamsOps._
import miksilo.lspprotocol.jsonRpc.JsonRpcMessage._
import play.api.libs.functional.syntax._
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.libs.json.Reads._
import play.api.libs.json._

import scala.collection.immutable.Seq

sealed abstract class JsonRpcMessage

// TODO this whole file came from https://github.com/dhpiggott/scala-json-rpc/blob/master/scala-json-rpc/src/main/scala/com/dhpcs/jsonrpc/JsonRpcMessage.scala
// We couldn't use the dependency because it doesn't publish a Scala.JS artifact. Find a way to put it back into a dependency
object JsonRpcMessage {

  final val Version = "2.0"

  sealed abstract class CorrelationId

  object CorrelationId {
    implicit final lazy val CorrelationIdFormat: Format[CorrelationId] = Format(
      Reads {
        case JsNull          => JsSuccess(NoCorrelationId)
        case JsNumber(value) => JsSuccess(NumericCorrelationId(value))
        case JsString(value) => JsSuccess(StringCorrelationId(value))
        case _               => JsError("error.expected.jsnumberorjsstring")
      },
      Writes {
        case NoCorrelationId             => JsNull
        case StringCorrelationId(value)  => JsString(value)
        case NumericCorrelationId(value) => JsNumber(value)
      }
    )
  }

  case object NoCorrelationId extends CorrelationId
  final case class NumericCorrelationId(value: BigDecimal) extends CorrelationId
  final case class StringCorrelationId(value: String) extends CorrelationId

  sealed abstract class Params

  object ParamsOps {
    implicit class RichSomeParamsOpt(val value: Option[SomeParams])
      extends AnyVal {
      def lift: Params = value.getOrElse(NoParams)
    }
    implicit class RichParams(val value: Params) extends AnyVal {
      def unlift: Option[SomeParams] = value match {
        case NoParams          => None
        case value: SomeParams => Some(value)
      }
    }
  }

  case object NoParams extends Params

  sealed abstract class SomeParams extends Params

  object SomeParams {
    implicit final lazy val SomeParamsFormat: Format[SomeParams] = Format(
      Reads {
        case jsValue: JsObject => JsSuccess(ObjectParams(jsValue))
        case jsArray: JsArray  => JsSuccess(ArrayParams(jsArray))
        case _ =>
          JsError(JsonValidationError(Seq("error.expected.jsobjectorjsarray")))
      },
      Writes {
        case ObjectParams(value) => value
        case ArrayParams(value)  => value
      }
    )
  }

  final case class ObjectParams(value: JsObject) extends SomeParams
  final case class ArrayParams(value: JsArray) extends SomeParams

  implicit final lazy val JsonRpcMessageFormat: Format[JsonRpcMessage] = Format(
    __.read(JsonRpcRequestMessage.JsonRpcRequestMessageFormat)
      .map(m => m: JsonRpcMessage) orElse
      __.read(JsonRpcRequestMessageBatch.JsonRpcRequestMessageBatchFormat)
        .map(m => m: JsonRpcMessage) orElse
      __.read(JsonRpcResponseMessage.JsonRpcResponseMessageFormat)
        .map(m => m: JsonRpcMessage) orElse
      __.read(JsonRpcResponseMessageBatch.JsonRpcResponseMessageBatchFormat)
        .map(m => m: JsonRpcMessage) orElse
      __.read(JsonRpcNotificationMessage.JsonRpcNotificationMessageFormat)
        .map(m => m: JsonRpcMessage) orElse
      Reads(
        _ =>
          JsError(
            "not a valid request, request batch, response, response batch " +
              "or notification message")),
    Writes {
      case jsonRpcRequestMessage: JsonRpcRequestMessage =>
        JsonRpcRequestMessage.JsonRpcRequestMessageFormat.writes(
          jsonRpcRequestMessage)
      case jsonRpcRequestMessageBatch: JsonRpcRequestMessageBatch =>
        JsonRpcRequestMessageBatch.JsonRpcRequestMessageBatchFormat.writes(
          jsonRpcRequestMessageBatch)
      case jsonRpcResponseMessage: JsonRpcResponseMessage =>
        JsonRpcResponseMessage.JsonRpcResponseMessageFormat.writes(
          jsonRpcResponseMessage)
      case jsonRpcResponseMessageBatch: JsonRpcResponseMessageBatch =>
        JsonRpcResponseMessageBatch.JsonRpcResponseMessageBatchFormat.writes(
          jsonRpcResponseMessageBatch)
      case jsonRpcNotificationMessage: JsonRpcNotificationMessage =>
        JsonRpcNotificationMessage.JsonRpcNotificationMessageFormat.writes(
          jsonRpcNotificationMessage)
    }
  )

}

sealed trait JsonRpcRequestOrNotificationMessage

object JsonRpcRequestOrNotificationMessage {
  implicit final lazy val JsonRpcRequestOrNotificationMessageFormat
  : OFormat[JsonRpcRequestOrNotificationMessage] =
    OFormat(
      Reads(jsValue =>
        (__ \ "id")(jsValue) match {
          case Nil =>
            jsValue
              .validate(
                JsonRpcNotificationMessage.JsonRpcNotificationMessageFormat)
              .map(m => m: JsonRpcRequestOrNotificationMessage)
          case _ =>
            jsValue
              .validate(JsonRpcRequestMessage.JsonRpcRequestMessageFormat)
              .map(m => m: JsonRpcRequestOrNotificationMessage)
        }),
      OWrites[JsonRpcRequestOrNotificationMessage] {
        case jsonRpcRequestMessage: JsonRpcRequestMessage =>
          JsonRpcRequestMessage.JsonRpcRequestMessageFormat.writes(
            jsonRpcRequestMessage)
        case jsonRpcNotificationMessage: JsonRpcNotificationMessage =>
          JsonRpcNotificationMessage.JsonRpcNotificationMessageFormat.writes(
            jsonRpcNotificationMessage)
      }
    )
}

final case class JsonRpcRequestMessage(method: String,
                                       params: Params,
                                       id: CorrelationId)
  extends JsonRpcMessage
    with JsonRpcRequestOrNotificationMessage

object JsonRpcRequestMessage {

  def apply(method: String,
            params: JsObject,
            id: CorrelationId): JsonRpcRequestMessage =
    JsonRpcRequestMessage(method, ObjectParams(params), id)

  def apply(method: String,
            params: JsArray,
            id: CorrelationId): JsonRpcRequestMessage =
    JsonRpcRequestMessage(method, ArrayParams(params), id)

  implicit final lazy val JsonRpcRequestMessageFormat
  : OFormat[JsonRpcRequestMessage] = (
    (__ \ "jsonrpc").format(verifying[String](_ == JsonRpcMessage.Version)) and
      (__ \ "method").format[String] and
      (__ \ "params").formatNullable[SomeParams] and
      (__ \ "id").format[CorrelationId]
    )(
    (_, method, params, id) => JsonRpcRequestMessage(method, params.lift, id),
    jsonRpcRequestMessage =>
      (JsonRpcMessage.Version,
        jsonRpcRequestMessage.method,
        jsonRpcRequestMessage.params.unlift,
        jsonRpcRequestMessage.id)
  )
}

final case class JsonRpcRequestMessageBatch(
                                             messages: Seq[JsonRpcRequestOrNotificationMessage])
  extends JsonRpcMessage {
  require(messages.nonEmpty)
}

object JsonRpcRequestMessageBatch {
  implicit final lazy val JsonRpcRequestMessageBatchFormat
  : Format[JsonRpcRequestMessageBatch] = Format(
    Reads
      .of[Seq[JsonRpcRequestOrNotificationMessage]](verifying(_.nonEmpty))
      .map(JsonRpcRequestMessageBatch(_)),
    Writes
      .of[Seq[JsonRpcRequestOrNotificationMessage]]
      .contramap(_.messages)
  )
}

sealed abstract class JsonRpcResponseMessage extends JsonRpcMessage {
  def id: CorrelationId
}

object JsonRpcResponseMessage {
  implicit final lazy val JsonRpcResponseMessageFormat
  : OFormat[JsonRpcResponseMessage] = OFormat(
    Reads(jsValue =>
      (__ \ "error")(jsValue) match {
        case Nil =>
          jsValue
            .validate(
              JsonRpcResponseSuccessMessage.JsonRpcResponseSuccessMessageFormat)
            .map(m => m: JsonRpcResponseMessage)
        case _ =>
          jsValue
            .validate(
              JsonRpcResponseErrorMessage.JsonRpcResponseErrorMessageFormat)
            .map(m => m: JsonRpcResponseMessage)
      }),
    OWrites[JsonRpcResponseMessage] {
      case jsonRpcResponseSuccessMessage: JsonRpcResponseSuccessMessage =>
        JsonRpcResponseSuccessMessage.JsonRpcResponseSuccessMessageFormat
          .writes(jsonRpcResponseSuccessMessage)
      case jsonRpcResponseErrorMessage: JsonRpcResponseErrorMessage =>
        JsonRpcResponseErrorMessage.JsonRpcResponseErrorMessageFormat.writes(
          jsonRpcResponseErrorMessage)
    }
  )
}

final case class JsonRpcResponseSuccessMessage(result: JsValue,
                                               id: CorrelationId)
  extends JsonRpcResponseMessage

object JsonRpcResponseSuccessMessage {
  implicit final lazy val JsonRpcResponseSuccessMessageFormat
  : OFormat[JsonRpcResponseSuccessMessage] = (
    (__ \ "jsonrpc").format(verifying[String](_ == JsonRpcMessage.Version)) and
      (__ \ "result").format[JsValue] and
      (__ \ "id").format[CorrelationId]
    )(
    (_, result, id) => JsonRpcResponseSuccessMessage(result, id),
    jsonRpcResponseSuccessMessage =>
      (JsonRpcMessage.Version,
        jsonRpcResponseSuccessMessage.result,
        jsonRpcResponseSuccessMessage.id)
  )
}

sealed abstract case class JsonRpcResponseErrorMessage(code: Int,
                                                       message: String,
                                                       data: Option[JsValue],
                                                       id: CorrelationId)
  extends JsonRpcResponseMessage

object JsonRpcResponseErrorMessage {

  implicit final lazy val JsonRpcResponseErrorMessageFormat
  : OFormat[JsonRpcResponseErrorMessage] = (
    (__ \ "jsonrpc").format(verifying[String](_ == JsonRpcMessage.Version)) and
      (__ \ "error" \ "code").format[Int] and
      (__ \ "error" \ "message").format[String] and
      // formatNullable allows the key and value to be completely absent
      (__ \ "error" \ "data").formatNullable[JsValue] and
      (__ \ "id").format[CorrelationId]
    )(
    (_, code, message, data, id) =>
      new JsonRpcResponseErrorMessage(code, message, data, id) {},
    jsonRpcResponseErrorMessage =>
      (JsonRpcMessage.Version,
        jsonRpcResponseErrorMessage.code,
        jsonRpcResponseErrorMessage.message,
        jsonRpcResponseErrorMessage.data,
        jsonRpcResponseErrorMessage.id)
  )

  final val ReservedErrorCodeFloor: Int = -32768
  final val ReservedErrorCodeCeiling: Int = -32000

  final val ParseErrorCode: Int = -32700
  final val InvalidRequestCode: Int = -32600
  final val MethodNotFoundCode: Int = -32601
  final val InvalidParamsCode: Int = -32602
  final val InternalErrorCode: Int = -32603
  final val ServerErrorCodeFloor: Int = -32099
  final val ServerErrorCodeCeiling: Int = -32000

  def parseError(exception: Throwable,
                 id: CorrelationId): JsonRpcResponseErrorMessage = rpcError(
    ParseErrorCode,
    message = "Parse error",
    meaning =
      "Invalid JSON was received by the server.\nAn error occurred on the " +
        "server while parsing the JSON text.",
    error = Some(JsString(exception.getMessage)),
    id
  )

  def invalidRequest(error: JsError,
                     id: CorrelationId): JsonRpcResponseErrorMessage =
    rpcError(
      InvalidRequestCode,
      message = "Invalid Request",
      meaning = "The JSON sent is not a valid Request object.",
      error = Some(JsError.toJson(error)),
      id
    )

  def methodNotFound(method: String,
                     id: CorrelationId): JsonRpcResponseErrorMessage = rpcError(
    MethodNotFoundCode,
    message = "Method not found",
    meaning = "The method does not exist / is not available.",
    error = Some(JsString(s"""The method "$method" is not implemented.""")),
    id
  )

  def invalidParams(error: JsError,
                    id: CorrelationId): JsonRpcResponseErrorMessage =
    rpcError(
      InvalidParamsCode,
      message = "Invalid params",
      meaning = "Invalid method parameter(s).",
      error = Some(JsError.toJson(error)),
      id
    )

  def internalError(error: Option[JsValue],
                    id: CorrelationId): JsonRpcResponseErrorMessage = rpcError(
    InternalErrorCode,
    message = "Internal error",
    meaning = "Internal JSON-RPC error.",
    error,
    id
  )

  def serverError(code: Int,
                  error: Option[JsValue],
                  id: CorrelationId): JsonRpcResponseErrorMessage = {
    require(code >= ServerErrorCodeFloor && code <= ServerErrorCodeCeiling)
    rpcError(
      code,
      message = "Server error",
      meaning = "Something went wrong in the receiving application.",
      error,
      id
    )
  }

  private[this] def rpcError(code: Int,
                             message: String,
                             meaning: String,
                             error: Option[JsValue],
                             id: CorrelationId): JsonRpcResponseErrorMessage =
    new JsonRpcResponseErrorMessage(
      code,
      message,
      data =
        Some(
          Json.obj(
            ("meaning" -> toJsFieldJsValueWrapper(meaning)) +:
              error.toSeq.map(error =>
                "error" -> toJsFieldJsValueWrapper(error)): _*
          )
        ),
      id
    ) {}

  def applicationError(code: Int,
                       message: String,
                       data: Option[JsValue],
                       id: CorrelationId): JsonRpcResponseErrorMessage = {
    require(code > ReservedErrorCodeCeiling || code < ReservedErrorCodeFloor)
    new JsonRpcResponseErrorMessage(
      code,
      message,
      data,
      id
    ) {}
  }
}

final case class JsonRpcResponseMessageBatch(
                                              messages: Seq[JsonRpcResponseMessage])
  extends JsonRpcMessage {
  require(messages.nonEmpty)
}

object JsonRpcResponseMessageBatch {
  // This prevents a cyclic dependency on JsonRpcMessageFormat that would
  // otherwise be chosen during implicit resolution due to Writes being
  // contravariant, combined with https://issues.scala-lang.org/browse/SI-2509.
  // See also: https://github.com/playframework/play-json/issues/51.
  import miksilo.lspprotocol.jsonRpc.JsonRpcResponseMessage.JsonRpcResponseMessageFormat
  implicit final lazy val JsonRpcResponseMessageBatchFormat
  : Format[JsonRpcResponseMessageBatch] = Format(
    Reads
      .of[Seq[JsonRpcResponseMessage]](verifying(_.nonEmpty))
      .map(JsonRpcResponseMessageBatch(_)),
    Writes
      .of[Seq[JsonRpcResponseMessage]]
      .contramap(_.messages)
  )
}

final case class JsonRpcNotificationMessage(method: String, params: Params)
  extends JsonRpcMessage
    with JsonRpcRequestOrNotificationMessage

object JsonRpcNotificationMessage {

  def apply(method: String, params: JsObject): JsonRpcNotificationMessage =
    JsonRpcNotificationMessage(method, ObjectParams(params))

  def apply(method: String, params: JsArray): JsonRpcNotificationMessage =
    JsonRpcNotificationMessage(method, ArrayParams(params))

  implicit final lazy val JsonRpcNotificationMessageFormat
  : OFormat[JsonRpcNotificationMessage] = (
    (__ \ "jsonrpc").format(verifying[String](_ == JsonRpcMessage.Version)) and
      (__ \ "method").format[String] and
      (__ \ "params").formatNullable[SomeParams]
    )(
    (_, method, params) => JsonRpcNotificationMessage(method, params.lift),
    jsonRpcNotificationMessage =>
      (JsonRpcMessage.Version,
        jsonRpcNotificationMessage.method,
        jsonRpcNotificationMessage.params.unlift)
  )
}