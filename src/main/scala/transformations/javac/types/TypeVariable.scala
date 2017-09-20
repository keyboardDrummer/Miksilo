package transformations.javac.types

import core.bigrammar.{BiGrammar, Keyword}
import core.particles.{CompilationState, DeltaWithGrammar}
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node}
import transformations.bytecode.types.TypeSkeleton

object TypeVariable extends DeltaWithGrammar {

  object TypeVariableKey extends Key
  object TypeVariableName extends Key
  override def transformGrammars(grammars: GrammarCatalogue, state: CompilationState): Unit = {
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
