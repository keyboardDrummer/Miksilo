package transformations.javac.types

import core.bigrammar.BiGrammar
import core.particles.ParticleWithGrammar
import core.particles.grammars.{GrammarCatalogue, KeyGrammar}
import core.particles.node.Node
import transformations.bytecode.types.{ObjectTypeC, TypeSkeleton}
import transformations.javac.types.MethodTypeC.MethodTypeKey

object TypeAbstraction extends ParticleWithGrammar {

  object TypeAbstractionKey
  object Body
  object Parameters
  object ParameterKey
  object ParameterName
  object ParameterClassBound
  object ParameterInterfaceBound

  def getBody(_type: Node): Node = {
    _type(TypeAbstraction.Body).asInstanceOf[Node]
  }

  def getParameters(_type: Node): Seq[Node] = {
    _type(TypeAbstraction.Parameters).asInstanceOf[Seq[Node]]
  }

  object TypeParametersGrammar
  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    transformByteCodeGrammar(grammars)
    transformJavaGrammar(grammars)
  }

  def transformJavaGrammar(grammars: GrammarCatalogue): Unit = {
    val variableGrammar: BiGrammar = identifier ^^ parseMap(ParameterKey, ParameterName)
    val parametersGrammar: BiGrammar = variableGrammar.some
    grammars.create(TypeParametersGrammar, ("<" ~> parametersGrammar <~ ">" <~ " ").option.optionToSeq)
  }

  object AbstractMethodTypeGrammar
  def transformByteCodeGrammar(grammars: GrammarCatalogue): Unit = {
    val byteCodeType = grammars.find(TypeSkeleton.ByteCodeTypeGrammar)
    val methodTypeGrammar = grammars.find(KeyGrammar(MethodTypeKey))
    val objectTypeGrammar = grammars.find(ObjectTypeC.ObjectTypeByteCodeGrammar)
    val classBound: BiGrammar = objectTypeGrammar
    val variableGrammar: BiGrammar = identifier ~ (":" ~> classBound.option) ~~ ((":" ~> classBound)*) ^^
      parseMap(ParameterKey, ParameterName, ParameterClassBound, ParameterInterfaceBound)
    val parametersGrammar: BiGrammar = variableGrammar.some
    val abstractMethodType = grammars.create(AbstractMethodTypeGrammar, ("<" ~> parametersGrammar <~ ">") ~ methodTypeGrammar ^^
      parseMap(TypeAbstractionKey, Parameters, Body))
    byteCodeType.addOption(abstractMethodType)
  }

  override def description: String = "Adds type abstraction or 'generics'."
}
