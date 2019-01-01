package deltas.solidity

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.{NodeField, NodeShape}

object UsingForDeclarationDelta extends DeltaWithGrammar {

  object Shape extends NodeShape
  object LibraryName extends NodeField
  object Type extends NodeField
  object Wildcard

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val typeGrammar = find(SolidityTypeDelta.Shape)
    val grammar = "using" ~~ identifier.as(LibraryName) ~~ "for" ~~
      ("*" ~> value(Wildcard) | typeGrammar).as(Type) ~ ";" asNode Shape
    find(SolidityContractDelta.Members).addAlternative(grammar)
  }

  override def description = "Add a using-for namespace member"

  override def dependencies = Set(SolidityContractDelta)
}
