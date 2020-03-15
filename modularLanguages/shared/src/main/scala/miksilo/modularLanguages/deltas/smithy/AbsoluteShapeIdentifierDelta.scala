package miksilo.modularLanguages.deltas.smithy

import miksilo.modularLanguages.core.deltas.DeltaWithGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.modularLanguages.core.node.{NodeField, NodeShape}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.modularLanguages.deltas.javac.classes.skeleton.HasConstraintsDelta

object AbsoluteShapeIdentifierDelta extends DeltaWithGrammar with HasConstraintsDelta {
  object Shape extends NodeShape
  object NameSpace extends NodeField
  object Relative extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val namespaceIdentifier = find(NamespaceDelta.Shape)
    val absoluteShapeId = namespaceIdentifier.as(NameSpace) ~ "#" ~ find(RelativeShapeIdentifierDelta.Shape).as(Relative) asLabelledNode Shape
    find(RelativeShapeIdentifierDelta.ShapeIdentifierGrammar).addAlternative(absoluteShapeId)
  }

  override def description = ""

  override def dependencies = Set(RelativeShapeIdentifierDelta, NamespaceDelta)

  override def shape = Shape

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
    // TODO implement
  }
}
