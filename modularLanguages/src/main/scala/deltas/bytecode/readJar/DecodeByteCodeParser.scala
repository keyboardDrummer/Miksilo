package deltas.bytecode.readJar

import java.io.BufferedInputStream

import core.deltas._
import core.deltas.path.PathRoot
import core.language.Compilation
import core.language.node.Node
import core.parsers.editorParsers.SourceRange
import core.smarts.FileDiagnostic
import deltas.bytecode.attributes.UnParsedAttribute
import deltas.bytecode.{ByteCodeFieldInfo, ByteCodeMethodInfo, ByteCodeSkeleton}
import languageServer.{Diagnostic, DiagnosticSeverity, HumanPosition}

object DecodeByteCodeParser extends DeltaWithPhase {

  override def description: String = "Decodes a binary bytecode classfile."

  override def dependencies: Set[Contract] = Set[Contract](UnParsedAttribute, ByteCodeFieldInfo, ByteCodeMethodInfo, ByteCodeSkeleton)

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    val uri = compilation.rootFile.get
    val inputStream = compilation.fileSystem.getFile(uri)
    val bis = new BufferedInputStream(inputStream)
    val inputBytes: Array[Byte] = Stream.continually(bis.read).takeWhile(-1 !=).map(_.toByte).toArray
    val parseResult: ClassFileParser.ParseResult[Node] = ClassFileParser.parse(inputBytes)
    if (parseResult.successful) {
      compilation.program = PathRoot(parseResult.get)
      parseResult.get.startOfUri = Some(uri)
    } else {
      val diagnostic = Diagnostic(SourceRange(HumanPosition(0, 0), HumanPosition(0, 0)), Some(DiagnosticSeverity.Error), "File was not a JVM classfile", None, None)
      compilation.diagnostics += FileDiagnostic(uri, diagnostic)
    }
  }
}
