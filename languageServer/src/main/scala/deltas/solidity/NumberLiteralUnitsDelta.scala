package deltas.solidity

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.{NodeField, NodeGrammar}
import deltas.expression.IntLiteralDelta

object NumberLiteralUnitsDelta extends DeltaWithGrammar {

  object NumberUnit extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val number = find(IntLiteralDelta.Shape).inner.asInstanceOf[NodeGrammar]
    val numberUnit = "wei" | "szabo" | "finney" | "ether" |
      "seconds" | "minutes" | "hours" | "days" | "weeks" | "years"
    number.inner = number.inner ~ numberUnit.spacedOption.as(NumberUnit)
  }

  override def description = "Adds solidity units to number literals"

  override def dependencies = Set(IntLiteralDelta)
}
