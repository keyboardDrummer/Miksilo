package deltas.bytecode.readJar

import java.io.BufferedInputStream

import core.deltas.{Contract, Delta, Language, ParseException}
import deltas.bytecode.attributes.UnParsedAttribute
import deltas.bytecode.{ByteCodeFieldInfo, ByteCodeMethodInfo, ByteCodeSkeleton}

import scala.util.{Failure, Success}

object DecodeByteCodeParser extends Delta {

  override def description: String = "Decodes a binary bytecode classfile."

  override def dependencies: Set[Contract] = Set[Contract](UnParsedAttribute, ByteCodeFieldInfo, ByteCodeMethodInfo, ByteCodeSkeleton)

  override def inject(state: Language): Unit = {
    state.buildParser = () => {
      val parser = ClassFileParser.classFileParser
      inputStream => {
        val bis = new BufferedInputStream(inputStream)
        val inputBytes = Stream.continually(bis.read).takeWhile(-1 !=).map(_.toByte)
        val parseResult = parser(new ArrayReader(0, inputBytes))
        if (!parseResult.successful)
          Failure(ParseException(parseResult.toString))
        else
          Success(parseResult.get)
      }
    }
  }
}
