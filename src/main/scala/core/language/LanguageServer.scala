package core.language

import core.deltas.path.{ChildPath, NodePath, PathRoot}
import core.language.node.{Position, SourceRange}
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
    declaration.position
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
      val inner = node.sources.find(kv => kv._2.contains(position)) //TODO Ik heb een SequencePath nodig met een sources veld, right? De elementen v.d. sequence krijgen echter geen SourceRange tijdens het parsen nu. Hoe te fixen? Volgens mij Nodes gewoon ook start en end position geven. Sequences van primitives hebben pech. Ander idee is dat sources een Seq[Range] wordt.
      inner.fold[SourceElement](node)(kv => {
        val location = node.getLocation(kv._1)
        location match {
          case childPath: ChildPath => getForNode(childPath)
          case _ => location
        }
      })
    }
    getForNode(PathRoot(getCompilation.program))
  }

  def getSource: InputStream = {
    getInput()
  }

  def getLines: Seq[String] = SourceUtils.streamToString(getSource).split("\n")
}
