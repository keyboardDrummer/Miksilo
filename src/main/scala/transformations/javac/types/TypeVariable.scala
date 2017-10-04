package transformations.javac.types

import core.bigrammar.{BiGrammar, Keyword}
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeClass, NodeField}
import core.particles.{DeltaWithGrammar, Language}
import transformations.bytecode.types.TypeSkeleton

object TypeVariable extends DeltaWithGrammar {

  object TypeVariableKey extends NodeClass
  object TypeVariableName extends NodeField
  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    transformByteCodeGrammar(grammars)
    transformJavaGrammar(grammars)
  }

  def transformJavaGrammar(grammars: GrammarCatalogue): Unit = {
    val typeGrammar = grammars.find(TypeSkeleton.JavaTypeGrammar)
    val variableGrammar: BiGrammar = identifier.asNode(TypeVariableKey, TypeVariableName)
    typeGrammar.addOption(variableGrammar)
  }

  def transformByteCodeGrammar(grammars: GrammarCatalogue): Unit = {
    val byteCodeType = grammars.find(TypeSkeleton.ByteCodeTypeGrammar)
    byteCodeType.addOption(new Keyword("T", false) ~> identifier <~ ";" asNode(TypeVariableKey, TypeVariableName))
  }

  def getTypeVariableName(node: Node): String = {
    node(TypeVariable.TypeVariableName).asInstanceOf[String]
  }

  override def description: String = "Adds references to type variables."
}
