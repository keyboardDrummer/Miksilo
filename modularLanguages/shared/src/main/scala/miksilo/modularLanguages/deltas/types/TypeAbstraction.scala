package miksilo.modularLanguages.deltas.javac.types

import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.deltas.{Contract, DeltaWithGrammar}
import miksilo.modularLanguages.core.deltas.grammars.{KeyGrammar, LanguageGrammars}
import miksilo.modularLanguages.core.node.{GrammarKey, Node, NodeField, NodeShape}
import miksilo.languageServer.core.language.Language
import miksilo.modularLanguages.deltas.bytecode.types.{QualifiedObjectTypeDelta, TypeSkeleton}
import miksilo.modularLanguages.deltas.javac.types.MethodTypeDelta.Shape

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
    create(TypeParametersGrammar, ("<" ~> parametersGrammar ~< ">" ~< printSpace).option.optionToSeq)
  }

  object AbstractMethodTypeGrammar extends GrammarKey
  def transformByteCodeGrammar(grammars: LanguageGrammars): Unit = {
    import grammars._
    val byteCodeType = find(TypeSkeleton.ByteCodeTypeGrammar)
    val methodTypeGrammar = find(KeyGrammar(Shape))
    val objectTypeGrammar = find(QualifiedObjectTypeDelta.byteCodeGrammarKey)
    val classBound: BiGrammar = objectTypeGrammar
    val variableGrammar: BiGrammar = identifier.as(ParameterName) ~
      (":" ~> classBound.option).as(ParameterClassBound) ~~
      ((":" ~> classBound)*).as(ParameterInterfaceBound) asNode ParameterKey
    val parametersGrammar: BiGrammar = variableGrammar.some
    val abstractMethodType = create(AbstractMethodTypeGrammar, (("<" ~> parametersGrammar.as(Parameters) ~< ">") ~ methodTypeGrammar.as(Body)).
      asNode(TypeAbstractionKey))
    byteCodeType.addAlternative(abstractMethodType)
  }

  override def description: String = "Adds type abstraction or 'generics'."

  override def dependencies: Set[Contract] = Set(TypeSkeleton)
}
