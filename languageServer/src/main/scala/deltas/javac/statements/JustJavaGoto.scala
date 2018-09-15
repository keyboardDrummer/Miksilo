package deltas.javac.statements

import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node.{Node, NodeField, NodeShape}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import deltas.bytecode.simpleBytecode.LabelledLocations
import deltas.statement.StatementDelta

object JustJavaGoto extends StatementInstance {
  override val shape = GotoKey

  object GotoKey extends NodeShape
  object Target extends NodeField

  def goto(label: String) = new Node(GotoKey, Target -> label)

  override def toByteCode(statement: NodePath, compilation: Compilation): Seq[Node] = {
    Seq(LabelledLocations.goTo(getTarget(statement.current)))
  }

  def getTarget(node: Node) = node(Target).asInstanceOf[String]

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val statementGrammar = find(StatementDelta.Grammar)
    statementGrammar.addAlternative("goto" ~~> identifier.as(Target) ~< ";" asNode GotoKey)
  }

  override def getNextStatements(obj: NodePath, labels: Map[Any, NodePath]): Set[NodePath] = Set(labels(getTarget(obj.current)))

  override def description: String = "Adds a goto statement"

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, statement: NodePath, parentScope: Scope): Unit = {
    val target = getTarget(statement)
    builder.resolve(target, statement, parentScope)
  }
}
