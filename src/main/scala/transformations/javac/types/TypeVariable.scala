package transformations.javac.types

import core.bigrammar.{BiGrammar, Keyword}
import core.particles.ParticleWithGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.node.Node
import transformations.bytecode.types.TypeSkeleton

object TypeVariable extends ParticleWithGrammar {

  object TypeVariableKey
  object TypeVariableName
  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    transformByteCodeGrammar(grammars)
    transformJavaGrammar(grammars)
  }

  def transformJavaGrammar(grammars: GrammarCatalogue): Unit = {
    val typeGrammar = grammars.find(TypeSkeleton.JavaTypeGrammar)
    val variableGrammar: BiGrammar = identifier ^^ parseMap(TypeVariableKey, TypeVariableName)
    typeGrammar.addOption(variableGrammar)
  }

  def transformByteCodeGrammar(grammars: GrammarCatalogue): Unit = {
    val byteCodeType = grammars.find(TypeSkeleton.ByteCodeTypeGrammar)
    byteCodeType.addOption(new Keyword("T", false) ~> identifier <~ ";" ^^ parseMap(TypeVariableKey, TypeVariableName))
  }

  def getTypeVariableName(node: Node): String = {
    node(TypeVariable.TypeVariableName).asInstanceOf[String]
  }

  override def description: String = "Adds references to type variables."
}
