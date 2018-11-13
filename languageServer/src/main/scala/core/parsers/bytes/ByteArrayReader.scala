package core.parsers.bytes

import scala.util.parsing.input.{Position, Reader}

class ByteArrayReader(offset: Int, bytes: Seq[Byte]) extends Reader[Byte] {
  override def first: Byte = bytes(offset)

  override def atEnd: Boolean = offset >= bytes.length

  override def rest = new ByteArrayReader(offset + 1, bytes)

  override def pos: Position = new Position {
    override def line: Int = 0

    override def column: Int = offset

    override protected def lineContents: String = "some byte"
  }
}
