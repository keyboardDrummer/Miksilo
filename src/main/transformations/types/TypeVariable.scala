package transformations.types

import core.bigrammar.Keyword
import core.particles.ParticleWithGrammar
import core.particles.grammars.GrammarCatalogue

object TypeVariable extends ParticleWithGrammar {

  object TypeVariableKey
  object TypeVariableName
  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val byteCodeType = grammars.find(TypeSkeleton.ByteCodeTypeGrammar)
    byteCodeType.addOption(new Keyword("T",false) ~> identifier <~ ";" ^^ parseMap(TypeVariableKey, TypeVariableName))

//    val typeGrammar = grammars.find(TypeSkeleton.JavaTypeGrammar)
//    val variableGrammar: BiGrammar = identifier ^^ parseMap(TypeVariableKey, TypeVariableName)
//    typeGrammar.addOption(variableGrammar)
  }

  override def description: String = "Adds references to type variables."
}
