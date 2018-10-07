package core.language

import core.language.node.Node
import core.smarts.{Constraint, Proofs}
import langserver.types.Diagnostic

import scala.collection.mutable
import scala.tools.nsc.interpreter.InputStream

trait FileSystem {
  def getFile(path: String): InputStream
}

class Compilation(val language: Language, val fileSystem: FileSystem, val rootFile: Option[String]) {
  var program: Node = _
  var proofs: Proofs = _
  var remainingConstraints: Seq[Constraint] = _
  var diagnostics: List[Diagnostic] = List.empty

  var output: String = _
  val state: mutable.Map[Any,Any] = mutable.Map.empty

  def runPhases(): Unit = {
    for(phase <- language.compilerPhases) {
      phase.action(this)
      if (diagnostics.nonEmpty)
        return
    }
  }
}

object EmptyFileSystem extends FileSystem {
  override def getFile(path: String): InputStream = throw new IllegalArgumentException(s"no file for path $path")
}

case class MultiFileSystem(files: Map[String, InputStream]) extends FileSystem {
  override def getFile(path: String): InputStream = files(path)
}

object Compilation
{
  def singleFile(language: Language, inputStream: InputStream): Compilation = {
    val filePath = "foo"
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