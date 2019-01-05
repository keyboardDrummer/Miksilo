package deltas.solidity

import core.bigrammar.BiGrammar
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.NodeShape
import deltas.statement.BlockDelta

object CustomModifierDelta extends DeltaWithGrammar {

  object Shape extends NodeShape

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val parameterList = find(SolidityFunctionDelta.Parameters)

    val blockGrammar: BiGrammar = find(BlockDelta.BlockGramar)
    val body = blockGrammar.as(SolidityFunctionDelta.Body)
    val optionalParameters = (parameterList | value(Seq.empty)).as(SolidityFunctionDelta.Parameters)
    val grammar = "modifier" ~~ identifier.as(SolidityFunctionDelta.Name) ~
      optionalParameters ~~ body
    find(SolidityContractDelta.Members).addAlternative(grammar)
  }

  override def description = "Adds solidity custom modifiers"

  override def dependencies = Set(SolidityFunctionDelta, BlockDelta)
}


