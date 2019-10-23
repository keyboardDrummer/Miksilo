package deltas.bytecode.readJar

import java.nio.ByteBuffer

import core.language.node.Node
import deltas.bytecode.constants.Utf8ConstantDelta

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.Reader

case class ByteReader(array: Array[Byte], _offset: Int) extends Reader[Byte] {
  def this(array: Array[Byte]) {
    this(array, 0)
  }

  override def drop(amount: Int): ByteReader = ByteReader(array, offset + amount)

  def first: Byte = array(offset)

  def rest: ByteReader = drop(1)

  override def atEnd: Boolean = offset == array.length

  override def pos = ???
}

trait ByteParserWriter extends Parsers {
  //type Input = ByteReader
  type Elem = Byte

  val ParseInteger = XBytes(4).map(bytes => bytes.getInt)
  val ParseFloat = XBytes(4).map(bytes => bytes.getFloat)
  val ParseDouble = XBytes(8).map(bytes => bytes.getDouble())
  val ParseLong = XBytes(8).map(bytes => bytes.getLong())
  val ParseByte = XBytes(1).map(bytes => bytes.get())
  val ParseShort = XBytes(2).map(bytes => bytes.getShort())
  val ParseUtf8: Parser[Node] = ParseShort.flatMap(length => parseString(length)).map(s => Utf8ConstantDelta.create(s))

  case class XBytes(amount: Int) extends Parser[ByteBuffer] {

    def apply(input: Input): ParseResult[ByteBuffer] = {
      Success(ByteBuffer.wrap(input.asInstanceOf[ByteReader].array, input.offset, amount), input.drop(amount))
    }
  }

  def parseString(length: Int) =
    XBytes(length).map(bytes => {
      new String(bytes.array(), bytes.position(), length, "UTF-8")
    })

  def elems(bytes: Seq[Byte]): Parser[Unit] = {
    XBytes(bytes.length).flatMap((parsedBytes: ByteBuffer) => {
      val destination = new Array[Byte](bytes.length)
      parsedBytes.get(destination)
      val result = if (destination sameElements bytes) {
        success(())
      }
      else {
        failure("parsed bytes '$parsedBytes' were not equal to expected bytes $bytes")
      }
      result
    })
  }
}
