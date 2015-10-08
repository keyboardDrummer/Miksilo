package transformations.bytecode.readJar

import scala.util.parsing.input.{Position, Reader}

class ArrayReader(offset: Int, bytes: Seq[Byte]) extends Reader[Byte] {
  override def first: Byte = bytes(offset)

  override def atEnd: Boolean = offset >= bytes.length

  override def pos: Position = new Position {
    override def line: Int = 0

    override def column: Int = offset

    override protected def lineContents: String = "some byte"
  }

  override def rest: Reader[Byte] = new ArrayReader(offset+1, bytes)
}
