package core.language

import core.deltas.path.{NodePath, PathRoot}
import core.language.exceptions.BadInputException
import core.language.node.{NodeLike, Position, SourceRange}
import core.smarts.{Proofs, SolveConstraintsDelta}
import util.SourceUtils

import scala.tools.nsc.interpreter.InputStream

class LanguageServer(getInput: () => InputStream, val language: Language) {

  private val constraintsPhaseIndex = language.compilerPhases.indexWhere(p => p.key == SolveConstraintsDelta)
  private val proofPhases = language.compilerPhases.take(constraintsPhaseIndex + 1)

  var compilation: Option[Compilation] = None

  def documentChanged(): Unit = {
    compilation = None
  }

  def isReference(position: Position): Boolean = {
    goOption(position).nonEmpty
  }

  def toPosition(row: Int, column: Int): Position = {
    val offsetOfLineStart = getLines.take(row - 1).map(l => l.length + 1).sum
    Position(offsetOfLineStart + column - 1)
  }

  def go(position: Position): SourceRange = {
    val declaration: Option[SourceElement] = goOption(position)
    declaration.get.position.get
  }

  private def goOption(position: Position): Option[SourceElement] = {
    for {
      proofs <- getProofs
      element = getSourceElement(position)
      declaration <- proofs.resolveLocation(element)
    } yield declaration
  }

  def compile(): Unit = {
    val compilation = new Compilation(language)
    try {
      compilation.program = language.parse(getSource).get
      for(phase <- proofPhases)
        phase.action(compilation)
      this.compilation = Some(compilation)
    } catch {
      case e: BadInputException =>
    }
  }

  def getCompilation: Option[Compilation] = {
    if (compilation.isEmpty)
      compile()
    compilation
  }

  def getProofs: Option[Proofs] = {
    getCompilation.map(c => c.proofs)
  }

  def getSourceElement(position: Position): SourceElement = {
    def getForNode(node: NodePath): SourceElement = {
      val childPositions = node.dataView.flatMap(kv => {
        val value = kv._2
        val childPaths = NodeLike.getChildNodeLikes[NodePath](value)
        if (childPaths.isEmpty) {
          Seq(node.getLocation(kv._1))
        } else {
          childPaths.map(child => getForNode(child))
        }
      })
      val childPosition = childPositions.find(kv => kv.position.exists(r => r.contains(position)))
      childPosition.fold[SourceElement](node)(x => x)
    }
    getForNode(PathRoot(getCompilation.get.program))
  }

  def getSource: InputStream = {
    getInput()
  }

  def getLines: Seq[String] = SourceUtils.streamToString(getSource).split("\n")
}
