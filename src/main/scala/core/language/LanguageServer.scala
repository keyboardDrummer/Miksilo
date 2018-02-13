package core.language

import core.deltas.path.{NodePath, PathRoot}
import core.language.node.{NodeLike, Position, SourceRange}
import core.smarts.{Proofs, SolveConstraintsDelta}
import util.SourceUtils

import scala.tools.nsc.interpreter.InputStream

class LanguageServer(getInput: () => InputStream, language: Language) {
  private val constraintsPhaseIndex = language.compilerPhases.indexWhere(p => p.key == SolveConstraintsDelta)
  private val proofPhases = language.compilerPhases.take(constraintsPhaseIndex + 1)

  var compilation: Compilation = _

  def go(position: Position): SourceRange = {
    val proofs = getProofs
    val element = getSourceElement(position)
    val declaration = proofs.scopeGraph.resolveLocation(element)
    declaration.position.get
  }

  def compile(): Unit = {
    compilation = new Compilation(language)
    compilation.program = language.parse(getSource).get
    for(phase <- proofPhases)
      phase.action(compilation)
  }

  def getCompilation: Compilation = {
    if (compilation == null)
      compile()
    compilation
  }

  def getProofs: Proofs = {
    getCompilation.proofs
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
    getForNode(PathRoot(getCompilation.program))
  }

  def getSource: InputStream = {
    getInput()
  }

  def getLines: Seq[String] = SourceUtils.streamToString(getSource).split("\n")
}
