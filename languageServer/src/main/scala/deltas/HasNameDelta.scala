package deltas

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.{NodeField, NodeLike, NodeWrapper}

object HasNameDelta extends DeltaWithGrammar {
  object Name extends NodeField

  trait HasName[T <: NodeLike] extends NodeWrapper[T] {
    def name: String = node.getValue(Name).asInstanceOf[String]
    def name_=(value: String): Unit = node(Name) = value
  }

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    grammars.create(Name, identifier.as(Name)) // TODO don't create a Labelled BiGrammar here.
  }

  override def description = "Introduces the concept of a name"

  override def dependencies = Set.empty
}
