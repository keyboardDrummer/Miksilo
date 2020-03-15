package miksilo.lspprotocol.jsonRpc

import scala.reflect.ClassTag

class JVMQueue[Item: ClassTag] extends SerialWorkQueue[Item] {
  private var running = false
  private val worker = new Thread(() => dequeueWorkItem())
  val queue = new CircularArrayBuffer[Item]

  override def modifyQueue(modifyQueue: CircularArrayBuffer[Item] => Unit): Unit = {
    val start = queue.synchronized({
      modifyQueue(queue)
      if (running) {
        false
      }
      else {
        running = true
        true
      }
    })
    if (start) {
      worker.run()
    }
  }

  def dequeueWorkItem(): Unit = {
    val message: Item = queue.synchronized({
      if (queue.isEmpty) {
        running = false
        null.asInstanceOf[Item]
      }
      else {
        queue.popLeft()
      }
    })
    if (message == null) {
      return
    }

    handler(message)
    dequeueWorkItem()
  }
}
