package deltas.solidity

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.{NodeField, NodeShape}
import deltas.javac.methods.MethodParameters
import deltas.solidity.SolidityFunctionDelta.Parameters

object EventDelta extends DeltaWithGrammar {

  object Shape extends NodeShape
  object ParameterIndexed extends NodeField
  object Anonymous extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val typeGrammar = find(SolidityTypeDelta.Shape)

    val parameter = typeGrammar.as(MethodParameters.Type) ~
      "indexed".spacedOption.as(ParameterIndexed) ~
      identifier.spacedOption.as(MethodParameters.Name) asNode MethodParameters.Shape
    val parameterList = create(Parameters, parameter.toParameterList)

    val grammar = "event" ~~ identifier ~ parameterList ~~ "anonymous".option.as(Anonymous) ~ ";" asNode Shape
    find(SolidityContractDelta.Members).addAlternative(grammar)
  }

  override def description = "Add events as contract members"

  override def dependencies = Set(SolidityContractDelta)
}
