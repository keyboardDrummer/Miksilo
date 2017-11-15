package deltas.javac.statements

import core.particles.{Compilation, Language, NodeGrammar}
import core.particles.grammars.LanguageGrammars
import core.particles.node.{Node, NodeClass}
import core.particles.path.Path
import deltas.bytecode.additions.LabelledLocations

object WhileContinueC extends StatementInstance {
  override val key = ContinueKey

  object ContinueKey extends NodeClass
  def continue = new Node(ContinueKey)

  override def toByteCode(statement: Path, compilation: Compilation): Seq[Node] = {
    val startLabel = WhileC.getRegistry(compilation).whileStartLabels(getWhileParent(statement))
    Seq(LabelledLocations.goTo(startLabel))
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    val statementGrammar = grammars.find(StatementSkeleton.StatementGrammar)
    statementGrammar.addOption(new NodeGrammar("continue;", ContinueKey))
  }

  override def description: String = "Jumps the program to the start of the loop."

  override def getNextStatements(obj: Path, labels: Map[Any, Path]): Set[Path] = {
    val _whileParent: Path = getWhileParent(obj)
    Set(labels(WhileC.startKey(_whileParent.current)))
  }

  def getWhileParent(obj: Path): Path = {
    val ancestors = obj.ancestors
    val _whileParent = ancestors.filter(ancestor => ancestor.clazz == WhileC.WhileKey).head
    _whileParent
  }
}
