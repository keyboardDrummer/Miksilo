package deltas.bytecode.readJar

import java.io.BufferedInputStream

import core.deltas._
import core.language.node.{Node, SourceRange}
import core.language.{Compilation, Language}
import core.parsers.bytes.ByteReader
import core.smarts.FileDiagnostic
import deltas.bytecode.attributes.UnParsedAttribute
import deltas.bytecode.{ByteCodeFieldInfo, ByteCodeMethodInfo, ByteCodeSkeleton}
import langserver.types.{Diagnostic, DiagnosticSeverity}
import languageServer.HumanPosition

object DecodeByteCodeParser extends DeltaWithPhase {

  override def description: String = "Decodes a binary bytecode classfile."

  override def dependencies: Set[Contract] = Set[Contract](UnParsedAttribute, ByteCodeFieldInfo, ByteCodeMethodInfo, ByteCodeSkeleton)

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    val parser = parserProp.get(compilation)
    val uri = compilation.rootFile.get
    val inputStream = compilation.fileSystem.getFile(uri)
    val bis = new BufferedInputStream(inputStream)
    val inputBytes: Array[Byte] = Stream.continually(bis.read).takeWhile(-1 !=).map(_.toByte).toArray
    val parseResult: ClassFileParser.ParseResult[Node] = parser.parse(ByteReader(inputBytes))
    if (parseResult.successful) {
      compilation.program = parseResult.get
      compilation.program.startOfUri = Some(uri)
    } else {
      val diagnostic = Diagnostic(SourceRange(HumanPosition(0, 0), HumanPosition(0, 0)),
        Some(DiagnosticSeverity.Error), None, None, "File was not a JVM classfile")
      compilation.diagnostics ++= List(FileDiagnostic(uri, diagnostic))
    }
  }

  val parserProp = new Property[ClassFileParser.Parser[Node]]()
  override def inject(language: Language): Unit = {
    super.inject(language)
    parserProp.add(language, ClassFileParser.classFileParser)
  }
}
