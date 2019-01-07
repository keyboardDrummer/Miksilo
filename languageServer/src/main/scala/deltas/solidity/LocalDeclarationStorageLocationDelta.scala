package deltas.solidity

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.NodeField
import deltas.statement.LocalDeclarationDelta

object LocalDeclarationStorageLocationDelta extends DeltaWithGrammar {

  object StorageLocation extends NodeField
  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val declaration = find(LocalDeclarationDelta.Shape)
    val name = declaration.findAs(LocalDeclarationDelta.Name)
    val storageLocation = (printSpace ~> ("memory" | "storage" | "calldata")).option.as(StorageLocation)
    name.set(storageLocation ~ name.value)
  }

  override def description = "Enables specifying a storage location for declarations"

  override def dependencies = Set(LocalDeclarationDelta)
}
