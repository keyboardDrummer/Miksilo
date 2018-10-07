package deltas.bytecode.readJar

import java.io.BufferedInputStream

import core.deltas._
import core.language.node.Node
import core.language.{Compilation, Language}
import deltas.bytecode.attributes.UnParsedAttribute
import deltas.bytecode.{ByteCodeFieldInfo, ByteCodeMethodInfo, ByteCodeSkeleton}

object DecodeByteCodeParser extends DeltaWithPhase {

  override def description: String = "Decodes a binary bytecode classfile."

  override def dependencies: Set[Contract] = Set[Contract](UnParsedAttribute, ByteCodeFieldInfo, ByteCodeMethodInfo, ByteCodeSkeleton)

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    val parser = parserProp.get(compilation)
    val inputStream = compilation.fileSystem.getFile(compilation.rootFile.get)
    val bis = new BufferedInputStream(inputStream)
    val inputBytes = Stream.continually(bis.read).takeWhile(-1 !=).map(_.toByte)
    val parseResult = parser(new ArrayReader(0, inputBytes))
    if (parseResult.successful)
      compilation.program = parseResult.get
    else
      compilation.diagnostics ++ List(DiagnosticUtil.getDiagnosticFromParseException(parseResult.toString))
  }

  val parserProp = new Property[ClassFileParser.Parser[Node]]()
  override def inject(language: Language): Unit = {
    super.inject(language)
    parserProp.add(language, ClassFileParser.classFileParser)
  }
}
