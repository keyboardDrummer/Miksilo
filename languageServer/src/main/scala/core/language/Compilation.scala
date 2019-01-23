package core.language

import core.deltas.path.PathRoot
import core.language.node.Node
import core.smarts.{Constraint, FileDiagnostic, Proofs}
import langserver.types.Diagnostic

import scala.collection.mutable
import scala.tools.nsc.interpreter.InputStream

class Compilation(val language: Language, val fileSystem: FileSystem, val rootFile: Option[String]) {
  var program: Node = _
  def root: PathRoot = PathRoot(program)
  var proofs: Proofs = _
  var remainingConstraints: Seq[Constraint] = _
  var diagnostics: List[FileDiagnostic] = List.empty

  var output: String = _
  val state: mutable.Map[Any,Any] = mutable.Map.empty

  def runPhases(): Unit = {
    for(phase <- language.compilerPhases) {
      phase.action(this)
    }
  }

  def diagnosticsForFile(uri: String): Seq[Diagnostic] = {
    diagnostics.
      filter(p => p.uri == uri).
      map(d => d.diagnostic)
  }
}

object Compilation
{
  val singleFileDefaultName = "singleFileDefault"
  def singleFile(language: Language, inputStream: InputStream): Compilation = {
    val filePath = singleFileDefaultName
    val result = new Compilation(language, new FileSystem {
      override def getFile(path: String): InputStream =
        if (path == filePath) inputStream
        else throw new IllegalArgumentException(s"no file for path $path")
    }, Some(filePath))

    result
  }

  def fromAst(language: Language, root: Node): Compilation = {
    val result = new Compilation(language, EmptyFileSystem, None)
    result.program = root
    result
  }
  implicit def toLanguage(compilation: Compilation): Language = compilation.language
}




