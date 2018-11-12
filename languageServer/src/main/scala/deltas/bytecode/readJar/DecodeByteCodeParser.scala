package deltas.bytecode.readJar

import java.io.BufferedInputStream

import core.deltas._
import core.language.node.Node
import core.language.{Compilation, Language}
import core.parsers.bytes.ByteArrayReader
import core.smarts.FileDiagnostic
import deltas.bytecode.attributes.UnParsedAttribute
import deltas.bytecode.{ByteCodeFieldInfo, ByteCodeMethodInfo, ByteCodeSkeleton}

import scala.collection.immutable

object DecodeByteCodeParser extends DeltaWithPhase {

  override def description: String = "Decodes a binary bytecode classfile."

  override def dependencies: Set[Contract] = Set[Contract](UnParsedAttribute, ByteCodeFieldInfo, ByteCodeMethodInfo, ByteCodeSkeleton)

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    val parser = parserProp.get(compilation)
    val uri = compilation.rootFile.get
    val inputStream = compilation.fileSystem.getFile(uri)
    val bis = new BufferedInputStream(inputStream)
    val inputBytes: immutable.Seq[Byte] = Stream.continually(bis.read).takeWhile(-1 !=).map(_.toByte)
    val parseResult: ClassFileParser.ParseResult[Node] = parser(new ByteArrayReader(0, inputBytes))
    if (parseResult.successful) {
      compilation.program = parseResult.get
      compilation.program.startOfUri = Some(uri)
    } else {
      compilation.diagnostics ++= List(FileDiagnostic(uri, DiagnosticUtil.getDiagnosticFromParseException(parseResult.toString)))
    }
  }

  val parserProp = new Property[ClassFileParser.Parser[Node]]()
  override def inject(language: Language): Unit = {
    super.inject(language)
    parserProp.add(language, ClassFileParser.classFileParser)
  }
}
