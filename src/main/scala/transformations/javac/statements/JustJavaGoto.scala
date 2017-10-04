package transformations.javac.statements

import core.particles.Language
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeClass, NodeField}
import core.particles.path.Path
import transformations.bytecode.additions.LabelledLocations

object JustJavaGoto extends StatementInstance {
  override val key = GotoKey

  object GotoKey extends NodeClass
  object Target extends NodeField

  def goto(label: String) = new Node(GotoKey, Target -> label)

  override def toByteCode(statement: Path, state: Language): Seq[Node] = {
    Seq(LabelledLocations.goTo(getTarget(statement.current)))
  }

  object JavaGotoGrammar

  def getTarget(node: Node) = node(Target).asInstanceOf[String]

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    val statementGrammar = grammars.find(StatementSkeleton.StatementGrammar)
    statementGrammar.addOption(grammars.create(JavaGotoGrammar, "goto" ~~> identifier ~< ";").asNode(GotoKey, Target))
  }

  override def getNextStatements(obj: Path, labels: Map[Any, Path]): Set[Path] = Set(labels(getTarget(obj.current)))

  override def getNextLabel(statement: Path): (Path, String) = super.getNextLabel(statement)

  override def description: String = "Adds a goto statement"
}
