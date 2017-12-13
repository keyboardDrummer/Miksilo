package deltas.bytecode.readJar

import java.io.{BufferedInputStream, InputStream}

import core.deltas.node.Node
import core.deltas.{Contract, Delta, Language}
import core.grammar.ParseException
import deltas.bytecode.attributes.UnParsedAttribute
import deltas.bytecode.{ByteCodeFieldInfo, ByteCodeMethodInfo, ByteCodeSkeleton}

object DecodeByteCodeParser extends Delta {

  override def description: String = "Decodes a binary bytecode classfile."

  override def dependencies: Set[Contract] = Set[Contract](UnParsedAttribute, ByteCodeFieldInfo, ByteCodeMethodInfo, ByteCodeSkeleton)

  override def inject(state: Language): Unit = {
    state._parse = decodeStream
  }

  def decodeStream(inputStream: InputStream): Node = {
    val bis = new BufferedInputStream(inputStream)
    val inputBytes = Stream.continually(bis.read).takeWhile(-1 !=).map(_.toByte)
    val parseResult = ClassFileParser.packratParser(new ArrayReader(0, inputBytes))
    if (!parseResult.successful)
      throw ParseException(parseResult.toString)

    parseResult.get
  }
}
