package deltas.javac.statements

import core.deltas.Compilation
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{GrammarKey, Node, NodeField, NodeShape}
import core.deltas.path.{ChildPath, NodePath}
import core.language.Language
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import deltas.bytecode.simpleBytecode.InferredStackFrames

object JustJavaLabel extends StatementInstance {
  override val key = LabelKey

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
    val statementGrammar = find(StatementSkeleton.StatementGrammar)
    statementGrammar.addOption(create(JavaLabelGrammar, "label" ~~> identifier.as(Name) ~< ";" asNode LabelKey))
  }

  override def description: String = "Adds a label statement"

  override def getLabels(obj: NodePath): Map[Any, NodePath] = {
    super.getLabels(obj) + (getName(obj.current) -> obj)
  }

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, statement: ChildPath, parentScope: Scope): Unit = {
    val label = getName(statement)
    builder.declare(label, statement.getLocation(Name), parentScope)
  }
}
