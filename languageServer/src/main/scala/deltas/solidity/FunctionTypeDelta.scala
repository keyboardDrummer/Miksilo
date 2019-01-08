package deltas.solidity

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import deltas.bytecode.types.TypeSkeleton
import deltas.javac.methods.MethodParameters

object FunctionTypeDelta extends DeltaWithGrammar {

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val typeGrammar = find(TypeSkeleton.JavaTypeGrammar)
    val storageLocation = find(StorageLocationDelta.StorageLocation)
    val parameter = typeGrammar.as(MethodParameters.Type) ~ storageLocation
    val parameterList = parameter.toParameterList
    val modifiers = (find(StateMutabilityDelta.Grammar) | "internal" | "external").many.as(SolidityFunctionDelta.Modifiers)
    val grammar = "function" ~~ parameterList ~~ modifiers ~~ "returns" ~~ parameterList
    typeGrammar.addAlternative(grammar)
  }

  override def description = "Add Solidity function types"

  override def dependencies = Set(TypeSkeleton)
}
