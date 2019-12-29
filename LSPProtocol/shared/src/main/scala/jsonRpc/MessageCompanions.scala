package com.dhpcs.jsonrpc

import com.dhpcs.jsonrpc.JsonRpcMessage._
import play.api.libs.json._

import scala.language.higherKinds
import scala.language.existentials
import scala.reflect.ClassTag

// TODO this whole file came from https://github.com/dhpiggott/scala-json-rpc/blob/master/scala-json-rpc/src/main/scala/com/dhpcs/jsonrpc/MessageCompanions.scala
// We couldn't use the dependency because it doesn't publish a Scala.JS artifact. Find a way to put it back into a dependency
trait CommandCompanion[A] {

  private[this] lazy val (methodReads, classWrites) = CommandFormats

  protected[this] val CommandFormats: (
    Map[String, Reads[_ <: A]],
      Map[Class[_], (String, OWrites[_ <: A])]
    )

  def read(jsonRpcRequestMessage: JsonRpcRequestMessage): JsResult[_ <: A] =
    methodReads.get(jsonRpcRequestMessage.method) match {
      case None => JsError(s"unknown method ${jsonRpcRequestMessage.method}")
      case Some(reads) =>
        jsonRpcRequestMessage.params match {
          case NoParams       => JsError("command parameters must be given")
          case ArrayParams(_) => JsError("command parameters must be named")
          case ObjectParams(value) =>
            reads.reads(value) match {
              // We do this just to reset the path in the success case.
              case JsError(invalid)    => JsError(invalid)
              case JsSuccess(valid, _) => JsSuccess(valid)
            }
        }
    }

  def write[B <: A](command: B, id: CorrelationId): JsonRpcRequestMessage = {
    val (method, writes) =
      classWrites.getOrElse(command.getClass,
        throw new IllegalArgumentException(
          s"No format found for ${command.getClass}"))
    val bWrites = writes.asInstanceOf[OWrites[B]]
    JsonRpcRequestMessage(method, bWrites.writes(command), id)
  }
}

trait ResponseCompanion[A] {

  private[this] lazy val (methodReads, classWrites) = ResponseFormats

  protected[this] val ResponseFormats: (
    Map[String, Reads[_ <: A]],
      Map[Class[_], (String, Writes[_ <: A])]
    )

  def read(jsonRpcResponseSuccessMessage: JsonRpcResponseSuccessMessage,
           method: String): JsResult[_ <: A] =
    methodReads(method).reads(jsonRpcResponseSuccessMessage.result) match {
      // We do this just to reset the path in the success case.
      case JsError(invalid)    => JsError(invalid)
      case JsSuccess(valid, _) => JsSuccess(valid)
    }

  def write[B <: A](response: B,
                    id: CorrelationId): JsonRpcResponseSuccessMessage = {
    val (_, writes) =
      classWrites.getOrElse(response.getClass,
        throw new IllegalArgumentException(
          s"No format found for ${response.getClass}"))
    val bWrites = writes.asInstanceOf[Writes[B]]
    JsonRpcResponseSuccessMessage(bWrites.writes(response), id)
  }
}

trait NotificationCompanion[A] {

  private[this] lazy val (methodReads, classWrites) = NotificationFormats

  protected[this] val NotificationFormats: (Map[String, Reads[_ <: A]],
    Map[Class[_],
      (String, OWrites[_ <: A])])

  def read(jsonRpcNotificationMessage: JsonRpcNotificationMessage)
  : JsResult[_ <: A] =
    methodReads.get(jsonRpcNotificationMessage.method) match {
      case None =>
        JsError(s"unknown method ${jsonRpcNotificationMessage.method}")
      case Some(reads) =>
        jsonRpcNotificationMessage.params match {
          case NoParams => JsError("notification parameters must be given")
          case ArrayParams(_) =>
            JsError("notification parameters must be named")
          case ObjectParams(value) =>
            reads.reads(value) match {
              // We do this just to reset the path in the success case.
              case JsError(invalid)    => JsError(invalid)
              case JsSuccess(valid, _) => JsSuccess(valid)
            }
        }
    }

  def write[B <: A](notification: B): JsonRpcNotificationMessage = {
    val (method, writes) =
      classWrites.getOrElse(notification.getClass,
        throw new IllegalArgumentException(
          s"No format found for ${notification.getClass}"))
    val bWrites = writes.asInstanceOf[OWrites[B]]
    JsonRpcNotificationMessage(method, bWrites.writes(notification))
  }
}

object Message {

  implicit class MessageFormat[A: ClassTag](
                                             methodAndFormat: (String, Format[A])) {
    private[Message] val classTag = implicitly[ClassTag[A]]
    private[Message] val (method, format) = methodAndFormat
  }

  object MessageFormats {
    def apply[A, W[_] <: Writes[_]](messageFormats: MessageFormat[_ <: A]*)
    : (Map[String, Reads[_ <: A]], Map[Class[_], (String, W[_ <: A])]) = {
      val methods = messageFormats.map(_.method)
      require(
        methods == methods.distinct,
        "Duplicate methods: " + methods.mkString(", ")
      )
      val classes = messageFormats.map(_.classTag.runtimeClass)
      require(
        classes == classes.distinct,
        "Duplicate classes: " + classes.mkString(", ")
      )
      val reads = messageFormats.map(
        messageFormat =>
          messageFormat.method ->
            messageFormat.format)
      val writes = messageFormats.map(
        messageFormat =>
          messageFormat.classTag.runtimeClass ->
            (messageFormat.method -> messageFormat.format
              .asInstanceOf[W[_ <: A]]))
      (reads.toMap, writes.toMap)
    }
  }

  def objectFormat[A](o: A): OFormat[A] = OFormat(
    _.validate[JsObject].map(_ => o),
    (_: A) => JsObject.empty
  )

}