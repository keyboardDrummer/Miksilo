package miksilo.modularLanguages.deltas.smithy

import miksilo.modularLanguages.core.deltas.DeltaWithGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.core.node.{NodeField, NodeShape}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.modularLanguages.deltas.FileWithMembersDelta
import miksilo.modularLanguages.deltas.javac.classes.skeleton.HasConstraintsDelta

object NamespaceDelta extends DeltaWithGrammar with HasConstraintsDelta {
  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val members = find(FileWithMembersDelta.Members)
    val namespaceIdentifier = identifier.manySeparated(".").as(Parts) asLabelledNode Shape
    val grammar = "namespace" ~~> namespaceIdentifier
    members.addAlternative(grammar)
  }

  object Shape extends NodeShape
  object Parts extends NodeField

  override def description = "Adds the namespace statement 'namespace com.foo.baz'"

  override def dependencies = Set(FileWithMembersDelta)

  override def shape = Shape

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
    // TODO implement
  }
}
