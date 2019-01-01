package deltas.solidity

import core.bigrammar.BiGrammar
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.{NodeField, NodeShape}
import deltas.expressions.VariableDelta
import deltas.javac.methods.MethodParameters
import deltas.javac.methods.call.CallDelta
import deltas.statement.BlockDelta

object SolidityFunctionDelta extends DeltaWithGrammar {

  object Shape extends NodeShape
  object Name extends NodeField
  object Parameters extends NodeField
  object ReturnValues extends NodeField
  object Body extends NodeField
  object Modifiers extends NodeField

  object ParameterStorageLocation extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val typeGrammar = find(TypeDelta.Grammar)
    val storageLocation: BiGrammar = "memory" | "storage" | "calldata"
    val parameter = typeGrammar.as(MethodParameters.Type) ~
      storageLocation.spacedOption.as(ParameterStorageLocation) ~
      identifier.spacedOption.as(MethodParameters.Name) asNode MethodParameters.Shape
    val parameterList = create(Parameters, parameter.toParameterList)

    val name = identifier.option.as(Name)

    val modifierInvocation = identifier.as(VariableDelta.Name) ~
      (find(CallDelta.CallArgumentsGrammar) | value(Seq.empty)).as(CallDelta.Arguments) asNode CallDelta.Shape

    val stateMutability = "pure" | "view" | "payable"
    val modifiers = create(Modifiers, (printSpace ~ (modifierInvocation | stateMutability | "external" | "public" | "internal" | "private")).many.as(Modifiers))
    val returnValues = printSpace ~ "returns" ~~ parameterList.as(ReturnValues) | value(Seq.empty).as(ReturnValues)
    val blockGrammar: BiGrammar = find(BlockDelta.Grammar)
    val body = (";" ~> value(Seq.empty) | blockGrammar).as(Body)
    val grammar = "function" ~~ name ~ parameterList.as(Parameters) ~ modifiers ~ returnValues ~~ body
    find(SolidityContractDelta.Members).addAlternative(grammar)
  }

  override def description = "Adds solidity functions"

  override def dependencies = Set(TypeDelta, BlockDelta)
}


