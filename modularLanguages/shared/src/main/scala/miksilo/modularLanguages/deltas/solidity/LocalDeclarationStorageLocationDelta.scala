package miksilo.modularLanguages.deltas.solidity

import miksilo.modularLanguages.core.bigrammar.GrammarReference
import miksilo.modularLanguages.core.deltas.DeltaWithGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.languageServer.core.language.Language
import miksilo.modularLanguages.deltas.statement.LocalDeclarationDelta
import miksilo.modularLanguages.deltas.HasNameDelta.Name

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
