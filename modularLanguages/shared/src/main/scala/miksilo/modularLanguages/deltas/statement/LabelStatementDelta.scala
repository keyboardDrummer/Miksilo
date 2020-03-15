package miksilo.modularLanguages.deltas.statement

import miksilo.modularLanguages.core.deltas.{DeltaWithGrammar, ShapeProperty}
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.modularLanguages.core.node.{GrammarKey, Node, NodeShape}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.modularLanguages.deltas.bytecode.simpleBytecode.LabelDelta

object LabelStatementDelta extends StatementInstance with DeltaWithGrammar {

  import miksilo.modularLanguages.deltas.HasNameDelta._

  def getUniqueLabel(compilation: Compilation, suggestion: String, path: NodePath) = {
    val container = path.ancestors.find(p => isLabelScope.get(compilation, p.shape).nonEmpty).get
    LabelDelta.getUniqueLabel("whileStart", container)
  }

  val isLabelScope = new ShapeProperty[Unit]()

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
    builder.declare(label, parentScope, statement.getField(Name))
  }
}
