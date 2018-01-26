package deltas.javac.statements

import core.deltas.Compilation
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeField, NodeShape}
import core.deltas.path.Path
import core.language.Language
import deltas.bytecode.simpleBytecode.LabelledLocations

object JustJavaGoto extends StatementInstance {
  override val key = GotoKey

  object GotoKey extends NodeShape
  object Target extends NodeField

  def goto(label: String) = new Node(GotoKey, Target -> label)

  override def toByteCode(statement: Path, compilation: Compilation): Seq[Node] = {
    Seq(LabelledLocations.goTo(getTarget(statement.current)))
  }

  def getTarget(node: Node) = node(Target).asInstanceOf[String]

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val statementGrammar = find(StatementSkeleton.StatementGrammar)
    statementGrammar.addOption("goto" ~~> identifier.as(Target) ~< ";" asNode GotoKey)
  }

  override def getNextStatements(obj: Path, labels: Map[Any, Path]): Set[Path] = Set(labels(getTarget(obj.current)))

  override def getNextLabel(statement: Path): (Path, String) = super.getNextLabel(statement)

  override def description: String = "Adds a goto statement"
}
