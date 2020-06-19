package miksilo.modularLanguages.deltas

import miksilo.modularLanguages.core.deltas.DeltaWithGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.languageServer.core.language.Language
import miksilo.modularLanguages.core.node.{NodeField, NodeLike, NodeWrapper}

object HasNameDelta extends DeltaWithGrammar {
  object Name extends NodeField {
    override def toString: String = "Name"
  }

  trait HasName[T <: NodeLike] extends NodeWrapper[T] {
    def name: String = node.getValue(Name).asInstanceOf[String]
    def name_=(value: String): Unit = node(Name) = value
  }

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    create(Name, identifier.as(Name))
  }

  override def description = "Introduces the concept of a name"

  override def dependencies = Set.empty
}
