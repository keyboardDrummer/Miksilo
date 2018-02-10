package deltas.javac.statements

import core.deltas.Compilation
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeField, NodeShape}
import core.deltas.path.{ChildPath, NodePath}
import core.language.Language
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import deltas.bytecode.simpleBytecode.LabelledLocations

object JustJavaGoto extends StatementInstance {
  override val key = GotoKey

  object GotoKey extends NodeShape
  object Target extends NodeField

  def goto(label: String) = new Node(GotoKey, Target -> label)

  override def toByteCode(statement: NodePath, compilation: Compilation): Seq[Node] = {
    Seq(LabelledLocations.goTo(getTarget(statement.current)))
  }

  def getTarget(node: Node) = node(Target).asInstanceOf[String]

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val statementGrammar = find(StatementSkeleton.StatementGrammar)
    statementGrammar.addOption("goto" ~~> identifier.as(Target) ~< ";" asNode GotoKey)
  }

  override def getNextStatements(obj: NodePath, labels: Map[Any, NodePath]): Set[NodePath] = Set(labels(getTarget(obj.current)))

  override def getNextLabel(statement: NodePath): (NodePath, String) = super.getNextLabel(statement)

  override def description: String = "Adds a goto statement"

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, statement: ChildPath, parentScope: Scope): Unit = {
    val target = getTarget(statement)
    builder.resolve(target, statement, parentScope)
  }
}
