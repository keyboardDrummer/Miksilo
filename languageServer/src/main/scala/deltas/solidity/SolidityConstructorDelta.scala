package deltas.solidity

import core.bigrammar.BiGrammar
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.{NodeField, NodeShape}
import deltas.expression.IntLiteralDelta
import deltas.statement.BlockDelta

object SolidityConstructorDelta extends DeltaWithGrammar { // TODO try to re-use other constructor delta's.

  object Shape extends NodeShape

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val parameterList = find(SolidityFunctionDelta.Parameters)

    val modifiers = find(SolidityFunctionDelta.Modifiers)
    val blockGrammar: BiGrammar = find(BlockDelta.BlockGramar)
    val body = blockGrammar.as(SolidityFunctionDelta.Body)
    val grammar = "constructor" ~ parameterList.as(SolidityFunctionDelta.Parameters) ~ modifiers ~~ body
    find(SolidityContractDelta.Members).addAlternative(grammar)
  }

  override def description = "Adds solidity constructors"

  override def dependencies = Set(SolidityFunctionDelta, BlockDelta)
}

object NumberLiteralUnitsDelta extends DeltaWithGrammar {

  object NumberUnit extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val number = find(IntLiteralDelta.Shape)
    val numberUnit = "wei" | "szabo" | "finney" | "ether" |
      "seconds" | "minutes" | "hours" | "days" | "weeks" | "years"
    number.inner = number.inner ~ numberUnit.spacedOption.as(NumberUnit)
  }

  override def description = "Adds solidity units to number literals"

  override def dependencies = Set(IntLiteralDelta)
}