package deltas.javac.statements

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeClass}
import core.deltas.path.{Path, PathRoot, SequenceElement}

object WhileContinueDelta extends DeltaWithGrammar {

  override def description: String = "Moves the control flow to the start of the while loop."

  override def dependencies: Set[Contract] = super.dependencies ++ Set(WhileDelta)

  override def inject(language: Language): Unit = {
    super.inject(language)
    val whilePhaseIndex = language.compilerPhases.indexWhere(p => p.key == WhileDelta)
    val (before, after) = language.compilerPhases.splitAt(whilePhaseIndex + 1)
    val newPhase = Phase(this, compilation => transformProgram(compilation.program, compilation))
    language.compilerPhases = before ++ Seq(newPhase) ++ after
  }

  def transformProgram(program: Node, compilation: Compilation): Unit = {
    PathRoot(program).visitClass(ContinueKey).foreach(path => transformContinue(path, compilation))
  }

  def transformContinue(continuePath: Path, compilation: Compilation): Unit = {
    val startLabel = continuePath.ancestors.flatMap(p => p.current.data.get(WhileDelta.WhileStart)).head.asInstanceOf[String]
    continuePath.asInstanceOf[SequenceElement].replaceWith(JustJavaGoto.goto(startLabel))
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    val statementGrammar = grammars.find(StatementSkeleton.StatementGrammar)
    statementGrammar.addOption(new NodeGrammar("continue;", ContinueKey))
  }

  object ContinueKey extends NodeClass
  def continue = new Node(ContinueKey)
}
