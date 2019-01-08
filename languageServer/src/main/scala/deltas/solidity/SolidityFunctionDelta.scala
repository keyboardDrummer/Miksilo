package deltas.solidity

import core.bigrammar.BiGrammar
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.NodeField
import deltas.bytecode.types.TypeSkeleton
import deltas.expression.VariableDelta
import deltas.javac.methods.call.CallDelta
import deltas.javac.methods.{MethodDelta, MethodParameters}
import deltas.statement.BlockDelta

object SolidityFunctionDelta extends DeltaWithGrammar {

  object ReturnValues extends NodeField
  object Modifiers extends NodeField

  object ParameterStorageLocation extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val typeGrammar = find(TypeSkeleton.JavaTypeGrammar)
    val storageLocation: BiGrammar = find(StorageLocationDelta.StorageLocation)
    val parameter = typeGrammar.as(MethodParameters.Type) ~
      storageLocation ~
      identifier.spacedOption.as(MethodParameters.Name) asNode MethodParameters.Shape
    val parameterList = create(MethodDelta.Parameters, parameter.toParameterList)

    val name = identifier.option.as(MethodDelta.Name)

    val modifierInvocation = identifier.as(VariableDelta.Name) ~
      (find(CallDelta.CallArgumentsGrammar) | value(Seq.empty)).as(CallDelta.Arguments) asNode CallDelta.Shape

    val stateMutability = find(StateMutabilityDelta.Grammar)
    val modifiers = create(Modifiers, (printSpace ~ (modifierInvocation | stateMutability | "external" | "public" | "internal" | "private")).many.as(Modifiers))
    val returnValues = (printSpace ~ "returns" ~~ parameterList | value(Seq.empty)).as(ReturnValues)
    val blockGrammar: BiGrammar = find(BlockDelta.BlockGramar)
    val body = (";" ~> value(Seq.empty) | blockGrammar).as(MethodDelta.Body)
    val grammar = "function" ~~ name ~ parameterList.as(MethodDelta.Parameters) ~ modifiers ~ returnValues ~~ body asLabelledNode MethodDelta.Shape
    find(SolidityContractDelta.Members).addAlternative(grammar)
  }

  override def description = "Adds solidity functions"

  override def dependencies = Set(TypeSkeleton, BlockDelta, StorageLocationDelta)
}


