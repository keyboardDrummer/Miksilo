package deltas.javac.types

import core.bigrammar.BiGrammar
import core.deltas.grammars.{LanguageGrammars, KeyGrammar}
import core.deltas.node.{GrammarKey, Node, NodeShape, NodeField}
import core.deltas.{DeltaWithGrammar, Language}
import deltas.bytecode.types.{ObjectTypeDelta, TypeSkeleton}
import deltas.javac.types.MethodType.MethodTypeKey

object TypeAbstraction extends DeltaWithGrammar {

  object TypeAbstractionKey extends NodeShape
  object Body extends NodeField
  object Parameters extends NodeField
  object ParameterKey extends NodeShape
  object ParameterName extends NodeField
  object ParameterClassBound extends NodeField
  object ParameterInterfaceBound extends NodeField

  def getBody(_type: Node): Node = {
    _type(TypeAbstraction.Body).asInstanceOf[Node]
  }

  def getParameters(_type: Node): Seq[Node] = {
    _type(TypeAbstraction.Parameters).asInstanceOf[Seq[Node]]
  }

  object TypeParametersGrammar extends GrammarKey
  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    transformByteCodeGrammar(grammars)
    transformJavaGrammar(grammars)
  }

  def transformJavaGrammar(grammars: LanguageGrammars): Unit = {
    import grammars._
    val variableGrammar: BiGrammar = identifier.as(ParameterName) asNode ParameterKey
    val parametersGrammar: BiGrammar = variableGrammar.some
    create(TypeParametersGrammar, ("<" ~> parametersGrammar ~< ">" ~< " ").option.optionToSeq)
  }

  object AbstractMethodTypeGrammar extends GrammarKey
  def transformByteCodeGrammar(grammars: LanguageGrammars): Unit = {
    import grammars._
    val byteCodeType = find(TypeSkeleton.ByteCodeTypeGrammar)
    val methodTypeGrammar = find(KeyGrammar(MethodTypeKey))
    val objectTypeGrammar = find(ObjectTypeDelta.ObjectTypeByteCodeGrammar)
    val classBound: BiGrammar = objectTypeGrammar
    val variableGrammar: BiGrammar = identifier.as(ParameterName) ~
      (":" ~> classBound.option).as(ParameterClassBound) ~~
      ((":" ~> classBound)*).as(ParameterInterfaceBound) asNode ParameterKey
    val parametersGrammar: BiGrammar = variableGrammar.some
    val abstractMethodType = create(AbstractMethodTypeGrammar, (("<" ~> parametersGrammar.as(Parameters) ~< ">") ~ methodTypeGrammar.as(Body)).
      asNode(TypeAbstractionKey))
    byteCodeType.addOption(abstractMethodType)
  }

  override def description: String = "Adds type abstraction or 'generics'."
}
