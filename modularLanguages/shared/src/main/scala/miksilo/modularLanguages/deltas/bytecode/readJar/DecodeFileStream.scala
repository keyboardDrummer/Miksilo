package miksilo.modularLanguages.deltas.bytecode.readJar

import miksilo.modularLanguages.core.deltas._
import miksilo.modularLanguages.core.deltas.path.PathRoot
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node.Node
import miksilo.editorParser.parsers.editorParsers.SourceRange
import miksilo.languageServer.core.smarts.FileDiagnostic
import miksilo.modularLanguages.deltas.bytecode.attributes.UnParsedAttribute
import miksilo.modularLanguages.deltas.bytecode.{ByteCodeFieldInfo, ByteCodeMethodInfo, ByteCodeSkeleton}
import miksilo.lspprotocol.lsp.{Diagnostic, DiagnosticSeverity, HumanPosition}
import java.io.ByteArrayOutputStream
import java.io.IOException
import java.io.InputStream

class DecodeFileStream(uri: String, fileStream: InputStream) extends DeltaWithPhase {

  override def description: String = "Decodes a binary bytecode classfile."

  override def dependencies: Set[Contract] = Set[Contract](UnParsedAttribute, ByteCodeFieldInfo, ByteCodeMethodInfo, ByteCodeSkeleton)

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    val inputBytes: Array[Byte] = readAllBytes(fileStream)
    val parseResult: ClassFileParser.ParseResult[Node] = ClassFileParser.parse(inputBytes)
    if (parseResult.successful) {
      compilation.program = PathRoot(parseResult.get)
      parseResult.get.startOfUri = Some(uri)
    } else {
      val diagnostic = Diagnostic(SourceRange(HumanPosition(0, 0), HumanPosition(0, 0)), Some(DiagnosticSeverity.Error), "File was not a JVM classfile", None, None)
      compilation.diagnostics += FileDiagnostic(uri, diagnostic)
    }
  }

  def readAllBytes(inputStream: InputStream): Array[Byte] = {
    val bufferLength = 4 * 0x400 // 4KB
    val buffer = new Array[Byte](bufferLength)
    var exception: IOException = null
    try {
      val outputStream = new ByteArrayOutputStream
      try {
        var readLength = 0
        while ( {
          readLength = inputStream.read(buffer, 0, bufferLength)
          readLength != -1
        }) {
          outputStream.write(buffer, 0, readLength)
        }
        outputStream.toByteArray
      } finally {
        if (outputStream != null) outputStream.close()
      }
    }
    catch {
      case e: IOException =>
        exception = e
        throw e
    }
    finally {
      if (exception == null) inputStream.close()
      else {
        try inputStream.close()
        catch {
          case e: IOException =>
            exception.addSuppressed(e)
        }
      }
    }
  }
}
