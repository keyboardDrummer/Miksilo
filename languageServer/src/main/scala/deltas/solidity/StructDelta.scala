package deltas.solidity

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.{NodeField, NodeShape}
import deltas.statement.LocalDeclarationDelta

object StructDelta extends DeltaWithGrammar {

  object Shape extends NodeShape
  object Name extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._

    val declaration = find(LocalDeclarationDelta.Shape)
    val grammar = "struct" ~~ identifier.as(Name) ~ "{" % declaration.manyVertical % "}"
    find(SolidityContractDelta.Members).addAlternative(grammar)
  }

  override def description = "Adds solidity structs"

  override def dependencies = Set(SolidityContractDelta)
}
