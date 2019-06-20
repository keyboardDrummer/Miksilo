package deltas.smithy

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.{Compilation, Language}
import core.language.node.{NodeField, NodeShape}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import deltas.FileWithMembersDelta
import deltas.javac.classes.skeleton.HasConstraintsDelta

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
