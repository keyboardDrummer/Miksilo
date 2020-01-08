package deltas.solidity

import core.bigrammar.GrammarReference
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import deltas.statement.LocalDeclarationDelta
import deltas.HasNameDelta.Name

object LocalDeclarationStorageLocationDelta extends DeltaWithGrammar {

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val declaration = find(LocalDeclarationDelta.Shape)
    val name = declaration.findAs(Name)
    val storageLocation = find(StorageLocationDelta.StorageLocation)
    name.previous.asInstanceOf[GrammarReference].set(storageLocation ~ name.value)
  }

  override def description = "Enables specifying a storage location for declarations"

  override def dependencies = Set(LocalDeclarationDelta, StorageLocationDelta)
}
