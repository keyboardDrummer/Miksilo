package jsonRpc

import scala.concurrent.Future
import scala.scalajs.js

trait JSMessageReader extends js.Object {
  def nextPayload(): js.Promise[String]
}

class FromJSMessageReader(js: JSMessageReader) extends MessageReader {
  override def nextPayload(): Future[String] = js.nextPayload().toFuture
}

trait JSMessageWriter extends js.Object {
  def write(msg: String): Unit
}

class FromJSMessageWriter(js: JSMessageWriter) extends MessageWriter {
  override def write(msg: String): Unit = js.write(msg)
}


