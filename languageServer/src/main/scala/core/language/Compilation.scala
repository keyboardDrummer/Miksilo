package core.language

//import core.deltas.path.PathRoot
import core.smarts.{Constraint, FileDiagnostic, Proofs}
import languageServer.{CodeAction, Diagnostic}

import scala.collection.mutable
import scala.tools.nsc.interpreter.InputStream

class Compilation(val language: Language, val fileSystem: FileSystem, val rootFile: Option[String]) {
  var program: SourceElement = _ // TODO type?
  var proofs: Proofs = _
  var remainingConstraints: Seq[Constraint] = _
  var diagnostics: Set[FileDiagnostic] = Set.empty
  var fixesPerDiagnostics: Map[Diagnostic, Seq[CodeAction]] = Map.empty

  def addDiagnosticsWithFixes(addition: Map[FileDiagnostic, Seq[CodeAction]]): Unit = {
    diagnostics ++= addition.keys
    fixesPerDiagnostics ++= addition.map(e => {
      val diagnostic = e._1.diagnostic
      (diagnostic.identifier, e._2)
    })
  }

  var output: String = _
  val state: mutable.Map[Any,Any] = mutable.Map.empty
  var stopped: Boolean = false

  def runPhases(): Unit = {
    for(phase <- language.compilerPhases) {
      if (stopped)
        return
      phase.action(this)
    }
  }

  def diagnosticsForFile(uri: String): Seq[Diagnostic] = {
    diagnostics.
      filter(p => p.uri == uri).
      map(d => d.diagnostic).toSeq
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

  def fromAst(language: Language, root: SourceElement): Compilation = { // TODO type root
    val result = new Compilation(language, EmptyFileSystem, None)
    result.program = root
    result
  }
  implicit def toLanguage(compilation: Compilation): Language = compilation.language
}




