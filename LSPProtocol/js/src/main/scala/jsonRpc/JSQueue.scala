package jsonRpc

import scala.reflect.ClassTag
import scala.scalajs.js.Dynamic.{global => g}

class JSQueue[Item: ClassTag] extends SerialWorkQueue[Item] {
  val queue = new CircularArrayBuffer[Item]

  override def modifyQueue(modify: CircularArrayBuffer[Item] => Unit): Unit = {
    modify(queue)
    g.setTimeout(() => {
      if (queue.nonEmpty) {
        val item = queue.popLeft()
        handler(item)
      }
    }, 1)
  }
}
