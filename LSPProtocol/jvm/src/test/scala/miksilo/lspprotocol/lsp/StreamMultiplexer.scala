package miksilo.lspprotocol.lsp

import java.io.OutputStream

class StreamMultiplexer(val outputs: Seq[OutputStream]) extends OutputStream {
  override def write(b: Int): Unit = outputs.foreach(next => next.write(b))

  override def write(b: Array[Byte], off: Int, len: Int): Unit = {
    outputs.foreach(next => next.write(b, off, len))
  }
}
