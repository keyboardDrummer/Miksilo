package lsp

import java.io.{IOException, InputStream, OutputStream}

import scala.collection.mutable

class InOutStream {

  val in: InputStream = new In()
  val out: OutputStream = new Out()

  val queue: mutable.Queue[Int] = mutable.Queue.empty

  class Out extends OutputStream {
    override def write(b: Int): Unit = {
      queue.enqueue(b)
      lock.synchronized {
        lock.notify()
      }
    }

    @throws[IOException]
    override def write(buffer: Array[Byte], offset: Int, length: Int): Unit = {
      if (buffer == null) throw new NullPointerException
      else if ((offset < 0) || (offset > buffer.length) || (length < 0) || ((offset + length) > buffer.length) || ((offset + length) < 0)) throw new IndexOutOfBoundsException
      else if (length == 0) return

      lock.synchronized {
        var charactersWritten = 0
        while (charactersWritten < length) {
          queue.enqueue(buffer(offset + charactersWritten))
          charactersWritten += 1
        }
        lock.notify()
      }
    }
  }

  class In extends InputStream {
    override def read(): Int = {
      lock.synchronized {
        while(queue.isEmpty) {
            lock.wait()
        }
      }
      queue.dequeue()
    }

    @throws[IOException]
    override def read(buffer: Array[Byte], offset: Int, length: Int): Int = {
      if (buffer == null) throw new NullPointerException
      else if (offset < 0 || length < 0 || length > buffer.length - offset) throw new IndexOutOfBoundsException
      else if (length == 0) return 0

      lock.synchronized {
        while(queue.isEmpty) {
          lock.wait()
        }
        var charactersRead = 0
        try
            while (charactersRead < length && queue.nonEmpty)
            {
              val character = queue.dequeue()
              buffer(offset + charactersRead) = character.toByte
              charactersRead += 1
            }
        catch {
          case _: IOException =>
        }
        charactersRead
      }
    }
  }

  val lock = new Object()
}
