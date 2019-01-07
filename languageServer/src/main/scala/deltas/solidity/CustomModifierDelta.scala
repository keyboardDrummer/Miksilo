package deltas.solidity

import core.bigrammar.BiGrammar
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.NodeShape
import deltas.javac.methods.MethodDelta
import deltas.statement.BlockDelta

object CustomModifierDelta extends DeltaWithGrammar {

  object Shape extends NodeShape

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val parameterList = find(MethodDelta.Parameters)

    val blockGrammar: BiGrammar = find(BlockDelta.BlockGramar)
    val body = blockGrammar.as(MethodDelta.Body)
    val optionalParameters = (parameterList | value(Seq.empty)).as(MethodDelta.Parameters)
    val grammar = "modifier" ~~ identifier.as(MethodDelta.Name) ~
      optionalParameters ~~ body
    find(SolidityContractDelta.Members).addAlternative(grammar)
  }

  override def description = "Adds solidity custom modifiers"

  override def dependencies = Set(SolidityFunctionDelta, BlockDelta)
}


