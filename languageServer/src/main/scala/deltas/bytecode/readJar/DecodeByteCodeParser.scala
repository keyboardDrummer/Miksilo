package deltas.bytecode.readJar

import java.io.BufferedInputStream

import core.deltas.DiagnosticUtil._
import core.deltas._
import core.language.Compilation
import core.language.node.Node
import core.parsers.strings.SourceRange
import core.smarts.FileDiagnostic
import deltas.bytecode.attributes.UnParsedAttribute
import deltas.bytecode.{ByteCodeFieldInfo, ByteCodeMethodInfo, ByteCodeSkeleton}
import languageServer.{Diagnostic, HumanPosition}
import org.eclipse.lsp4j.DiagnosticSeverity

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
      compilation.program = parseResult.get
      compilation.program.startOfUri = Some(uri)
    } else {
      val diagnostic = Diagnostic(SourceRange(HumanPosition(0, 0), HumanPosition(0, 0)),
        "File was not a JVM classfile", Some(DiagnosticSeverity.Error))
      compilation.diagnostics += FileDiagnostic(uri, diagnostic)
    }
  }
}
