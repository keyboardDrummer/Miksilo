package miksilo.modularLanguages.deltas.statement

import miksilo.modularLanguages.core.deltas.DeltaWithGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.modularLanguages.core.node.{Node, NodeField, NodeShape}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope

object GotoStatementDelta extends DeltaWithGrammar with StatementInstance {
  override val shape = Shape

  object Shape extends NodeShape
  object Target extends NodeField

  def neww(label: String) = new Node(Shape, Target -> label)

  def getTarget(node: Node): String = node(Target).asInstanceOf[String]

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val statementGrammar = find(StatementDelta.Grammar)
    statementGrammar.addAlternative("goto" ~~> identifier.as(Target) ~< ";" asLabelledNode Shape)
  }

  override def getControlFlowGraph(language: Language, statement: NodePath, labels: Map[Any, NodePath]): ControlFlowGraph = {
    labels.get(getTarget(statement.current)).fold(ControlFlowGraph.empty)(ControlFlowGraph.singleton)
  }

  override def description: String = "Adds a goto statement"

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, statement: NodePath, parentScope: Scope): Unit = {
    val target = getTarget(statement)
    builder.resolve(target, parentScope, statement)
  }
}
