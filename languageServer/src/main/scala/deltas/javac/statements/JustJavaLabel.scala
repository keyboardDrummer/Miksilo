package deltas.javac.statements

import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node.{GrammarKey, Node, NodeField, NodeShape}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import deltas.bytecode.simpleBytecode.InferredStackFrames
import deltas.statement.{StatementDelta, StatementInstance}

object JustJavaLabel extends ByteCodeStatementInstance with StatementInstance {
  override val shape = LabelKey

  object LabelKey extends NodeShape
  object Name extends NodeField

  def label(name: String) = new Node(LabelKey, Name -> name)

  override def toByteCode(statement: NodePath, compilation: Compilation): Seq[Node] = {
    Seq(InferredStackFrames.label(getName(statement.current)))
  }

  def getName(statement: Node) = statement(Name).asInstanceOf[String]

  object JavaLabelGrammar extends GrammarKey

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val statementGrammar = find(StatementDelta.Grammar)
    statementGrammar.addAlternative(create(JavaLabelGrammar, "label" ~~> identifier.as(Name) ~< ";" asNode LabelKey))
  }

  override def description: String = "Adds a label statement"

  override def getLabels(obj: NodePath): Map[Any, NodePath] = {
    super.getLabels(obj) + (getName(obj.current) -> obj)
  }

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, statement: NodePath, parentScope: Scope): Unit = {
    val label = getName(statement)
    builder.declare(label, parentScope, statement.getLocation(Name))
  }
}
