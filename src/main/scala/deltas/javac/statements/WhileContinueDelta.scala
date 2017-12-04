package deltas.javac.statements

import core.deltas.{Compilation, Contract, Language, NodeGrammar}
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeClass}
import core.deltas.path.Path
import deltas.bytecode.simpleBytecode.LabelledLocations

object WhileContinueDelta extends StatementInstance {
  override val key = ContinueKey

  object ContinueKey extends NodeClass
  def continue = new Node(ContinueKey)


  override def dependencies: Set[Contract] = super.dependencies ++ Set(WhileDelta)

  override def toByteCode(statement: Path, compilation: Compilation): Seq[Node] = {
    val startLabel = WhileDelta.getState(compilation).whileStartLabels(getWhileParent(statement))
    Seq(LabelledLocations.goTo(startLabel))
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    val statementGrammar = grammars.find(StatementSkeleton.StatementGrammar)
    statementGrammar.addOption(new NodeGrammar("continue;", ContinueKey))
  }

  override def description: String = "Jumps the program to the start of the loop."

  override def getNextStatements(obj: Path, labels: Map[Any, Path]): Set[Path] = {
    val _whileParent: Path = getWhileParent(obj)
    Set(labels(WhileDelta.startKey(_whileParent.current)))
  }

  def getWhileParent(obj: Path): Path = {
    obj.findAncestorClass(WhileDelta.key)
  }
}
