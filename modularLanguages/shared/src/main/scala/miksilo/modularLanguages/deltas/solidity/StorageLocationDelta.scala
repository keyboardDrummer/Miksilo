package miksilo.modularLanguages.deltas.solidity

import miksilo.modularLanguages.core.deltas.DeltaWithGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.languageServer.core.language.Language
import miksilo.modularLanguages.core.node.NodeField

object StorageLocationDelta extends DeltaWithGrammar {
  object StorageLocation extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val storageLocation = ("memory" | "storage" | "calldata").spacedOption.as(StorageLocation)
    create(StorageLocation, storageLocation)
  }

  override def description = "Defines the a Solidity storage location"

  override def dependencies = Set.empty
}
