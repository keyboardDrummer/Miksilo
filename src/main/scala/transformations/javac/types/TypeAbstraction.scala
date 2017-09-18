package transformations.javac.types

import core.bigrammar.BiGrammar
import core.particles.{CompilationState, DeltaWithGrammar}
import core.particles.grammars.{GrammarCatalogue, KeyGrammar}
import core.particles.node.{Key, Node, NodeField}
import transformations.bytecode.types.{ObjectTypeC, TypeSkeleton}
import transformations.javac.types.MethodType.MethodTypeKey

object TypeAbstraction extends DeltaWithGrammar {

  object TypeAbstractionKey extends Key
  object Body extends Key
  object Parameters extends Key
  object ParameterKey extends Key
  object ParameterName extends NodeField
  object ParameterClassBound extends NodeField
  object ParameterInterfaceBound extends NodeField

  def getBody(_type: Node): Node = {
    _type(TypeAbstraction.Body).asInstanceOf[Node]
  }

  def getParameters(_type: Node): Seq[Node] = {
    _type(TypeAbstraction.Parameters).asInstanceOf[Seq[Node]]
  }

  object TypeParametersGrammar
  override def transformGrammars(grammars: GrammarCatalogue, state: CompilationState): Unit = {
    transformByteCodeGrammar(grammars)
    transformJavaGrammar(grammars)
  }

  def transformJavaGrammar(grammars: GrammarCatalogue): Unit = {
    val variableGrammar: BiGrammar = identifier.as(ParameterName) asNode ParameterKey
    val parametersGrammar: BiGrammar = variableGrammar.some
    grammars.create(TypeParametersGrammar, ("<" ~> parametersGrammar <~ ">" <~ " ").option.optionToSeq)
  }

  object AbstractMethodTypeGrammar
  def transformByteCodeGrammar(grammars: GrammarCatalogue): Unit = {
    val byteCodeType = grammars.find(TypeSkeleton.ByteCodeTypeGrammar)
    val methodTypeGrammar = grammars.find(KeyGrammar(MethodTypeKey))
    val objectTypeGrammar = grammars.find(ObjectTypeC.ObjectTypeByteCodeGrammar)
    val classBound: BiGrammar = objectTypeGrammar
    val variableGrammar: BiGrammar = identifier.as(ParameterName) ~
      (":" ~> classBound.option).as(ParameterClassBound) ~~
      ((":" ~> classBound)*).as(ParameterInterfaceBound) asNode ParameterKey
    val parametersGrammar: BiGrammar = variableGrammar.some
    val abstractMethodType = grammars.create(AbstractMethodTypeGrammar, (("<" ~> parametersGrammar <~ ">") ~ methodTypeGrammar).
      asNode(TypeAbstractionKey, Parameters, Body))
    byteCodeType.addOption(abstractMethodType)
  }

  override def description: String = "Adds type abstraction or 'generics'."
}
