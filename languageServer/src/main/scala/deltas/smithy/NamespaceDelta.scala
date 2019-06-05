package deltas.smithy

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.{NodeField, NodeShape}
import deltas.FileWithMembersDelta

object NamespaceDelta extends DeltaWithGrammar {
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
}
