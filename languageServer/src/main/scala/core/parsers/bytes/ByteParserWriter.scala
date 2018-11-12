package core.parsers.bytes

import java.nio.ByteBuffer

import core.language.node.Node
import deltas.bytecode.constants.Utf8ConstantDelta

import scala.util.parsing.combinator.Parsers

trait ByteParserWriter extends Parsers {
  type Elem = Byte

  val parseUtf8: Parser[Node] = ParseShort.flatMap(length => new ParseString(length)).map(s => Utf8ConstantDelta.create(s))
  class ParseString(length: Int) extends Parser[String] {
     override def apply(in: Input): ParseResult[String] = {
       val (bytes, rest) = splitInput(in, length)
       Success(new String(bytes.toArray, 0, length, "UTF-8"), rest)
     }
   }

   object ParseInteger extends Parser[Int] {
     override def apply(in: Input): ParseResult[Int] = {
       val (bytes, rest) = splitInput(in, 4)
       Success(ByteBuffer.wrap(bytes.toArray).getInt, rest)
     }
   }

   object ParseFloat extends Parser[Float] {
     override def apply(in: Input): ParseResult[Float] = {
       val (bytes, rest) = splitInput(in, 4)
       Success(ByteBuffer.wrap(bytes.toArray).getFloat, rest)
     }
   }

  object ParseDouble extends Parser[Double]
  {
    override def apply(in: Input): ParseResult[Double] = {
      val (bytes, rest) = splitInput(in, 8)
      Success(ByteBuffer.wrap(bytes.toArray).getDouble, rest)
    }
  }

  object ParseLong extends Parser[Long] {
    override def apply(in: Input): ParseResult[Long] = {
      val (bytes, rest) = splitInput(in, 8)
      Success(ByteBuffer.wrap(bytes.toArray).getLong, rest)
    }
  }

  object ParseByte extends Parser[Byte] {
     override def apply(in: Input): ParseResult[Elem] = {
       Success(in.first, in.rest)
     }
   }

   def splitInput(in: Input, index: Int): (List[Byte], Input) = index match {
     case 0 => (List.empty, in)
     case n =>
       val (nextList, nextInput) = splitInput(in.rest, index - 1)
       (in.first :: nextList, nextInput)
   }

   object ParseShort extends Parser[Short] {
     override def apply(in: Input): ParseResult[Short] = {
       val (bytes, rest) = splitInput(in, 2)
       Success(ByteBuffer.wrap(bytes.toArray).getShort, rest)
     }
   }

   def elems(elements: Seq[Byte]): Parser[Unit] = elements.headOption match {
     case Some(head) => elem(head) ~> elems(elements.drop(1))
     case None => success(())
   }
 }
