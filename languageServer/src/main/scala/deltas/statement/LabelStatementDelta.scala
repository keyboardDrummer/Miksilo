package deltas.statement

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node.{GrammarKey, Node, NodeShape}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import deltas.bytecode.simpleBytecode.LabelDelta

object LabelStatementDelta extends StatementInstance with DeltaWithGrammar {

  import deltas.HasNameDelta._

  def getUniqueLabel(compilation: Compilation, suggestion: String, path: NodePath) = {
    val container = path.ancestors.find(p => !StatementDelta.instances.get(compilation).contains(p.shape)).get
    LabelDelta.getUniqueLabel("whileStart", container)
  }

  override val shape = Shape

  object Shape extends NodeShape

  def neww(name: String) = new Node(Shape, Name -> name)

  def getName(statement: Node) = statement(Name).asInstanceOf[String]

  object JavaLabelGrammar extends GrammarKey

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val statementGrammar = find(StatementDelta.Grammar)
    statementGrammar.addAlternative(create(JavaLabelGrammar, "label" ~~> find(Name) ~< ";" asNode Shape))
  }

  override def description: String = "Adds a label statement"

  override def getLabels(language: Language, obj: NodePath): Map[Any, NodePath] = {
    super.getLabels(language, obj) + (getName(obj.current) -> obj)
  }

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, statement: NodePath, parentScope: Scope): Unit = {
    val label = getName(statement)
    builder.declare(label, parentScope, statement.getSourceElement(Name))
  }
}
