package cloudformation

import jsonRpc.{MessageReader, MessageWriter}

import scala.scalajs.js
import scala.scalajs.js.annotation.ScalaJSDefined

trait JSMessageReader extends js.Object {
  def nextPayload(): js.Promise[String]
}

class FromJSMessageReader(js: JSMessageReader) extends MessageReader {
  override def nextPayload() = js.nextPayload().toFuture
}

trait JSMessageWriter extends js.Object {
  def write(msg: String): Unit
}

class FromJSMessageWriter(js: JSMessageWriter) extends MessageWriter {
  override def write(msg: String): Unit = js.write(msg)
}


