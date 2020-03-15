package miksilo.languageServer.core.language

import miksilo.languageServer.core.smarts.{Constraint, FileDiagnostic, Proofs}
import languageServer.SourcePath
import miksilo.editorParser.parsers.core.{Metrics, NoMetrics}
import miksilo.lspprotocol.lsp.{CodeAction, Diagnostic}

import scala.collection.mutable
import scala.language.implicitConversions

class CompilationCache(val language: Language, val fileSystem: FileSystem,
                       var metrics: Metrics = NoMetrics) {
  val state = new mutable.HashMap[Any, Any]
}

trait CompilationState
object Stopped extends CompilationState
object Started extends CompilationState
object NotStarted extends CompilationState

class Compilation(val cache: CompilationCache, var rootFile: Option[String]) {

  def metrics = cache.metrics
  def fileSystem = cache.fileSystem
  def language = cache.language

  var program: SourcePath = _

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
  val data: mutable.Map[Any, Any] = new mutable.HashMap
  private var state: CompilationState = NotStarted

  def isStarted: Boolean = state != NotStarted
  def hasStopped = state == Stopped
  def stop() = state = Stopped

  def runPhases(): Unit = {
    state = Started
    for(phase <- cache.language.compilerPhases) {
      if (hasStopped)
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
  def singleFile(language: Language, input: String): Compilation = {
    val filePath = singleFileDefaultName
    new Compilation(new CompilationCache(language, InMemoryFileSystem(Map(filePath -> input))), Some(filePath))
  }

  def fromAst(language: Language, root: SourcePath): Compilation = {
    val result = new Compilation(new CompilationCache(language, EmptyFileSystem), None)
    result.program = root
    result
  }
  implicit def toLanguage(compilation: Compilation): Language = compilation.cache.language
}




