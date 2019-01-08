package deltas.solidity

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.NodeField

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
