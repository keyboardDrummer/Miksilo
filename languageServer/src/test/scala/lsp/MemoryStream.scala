package lsp

import java.io.InputStream

class MemoryStream extends InputStream {
  var data: List[Array[Byte]] = List(Array.emptyByteArray)
  var index: Int = 0
  val lock = new Object()

  override def read(buffer: Array[Byte]): Int = {
    val result = super.read(buffer)
    if (result == -1) {
      lock.synchronized {
        lock.wait()
      }
      read(buffer)
    } else
      result
  }

  override def read(): Int = {
    if (data.isEmpty) {
      -1
    } else {
      val head = data.head
      if (head.length > index) {
        val result = head(index)
        index += 1
        result
      } else {
        data = data.tail
        index = 0
        read()
      }
    }
  }

  def add(newData: Array[Byte]): Unit = {
    data = data ++ List(newData)
    lock.synchronized {
      lock.notifyAll()
    }
  }
}
