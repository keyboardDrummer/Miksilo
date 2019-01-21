package deltas.solidity

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.{NodeField, NodeShape}

object EnumDelta extends DeltaWithGrammar {

  object Shape extends NodeShape
  object Name extends NodeField
  object Values extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._

    val grammar = "enum" ~~ identifier.as(Name) ~ "{" ~ identifier.manySeparated("," ~ printSpace).as(Values) ~ "}" asNode Shape
    find(SolidityContractDelta.Members).addAlternative(grammar)
  }

  override def description = "Adds enums"

  override def dependencies = Set(SolidityContractDelta)
}
