package deltas.javac.types

import core.bigrammar.grammars.Keyword
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.node.{Node, NodeField, NodeLike, NodeShape}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.bytecode.types.{HasType, TypeSkeleton}

object TypeVariableDelta extends DeltaWithGrammar with HasType {

  override def description: String = "Adds references to type variables."

  object Shape extends NodeShape
  object TypeVariableName extends NodeField
  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    transformByteCodeGrammar(grammars)
    transformJavaGrammar(grammars)
  }

  def transformJavaGrammar(grammars: LanguageGrammars): Unit = {
    //TODO FIX?!
//    import grammars._
//    val typeGrammar = find(TypeSkeleton.JavaTypeGrammar)
//    val variableGrammar: BiGrammar = identifier.as(TypeVariableName).asNode(Shape)
//    typeGrammar.addOption(variableGrammar)
  }

  def transformByteCodeGrammar(grammars: LanguageGrammars): Unit = {
    import grammars._
    val byteCodeType = find(TypeSkeleton.ByteCodeTypeGrammar)
    byteCodeType.addOption(Keyword("T", false) ~> identifier.as(TypeVariableName) ~< ";" asNode Shape)
  }

  def getTypeVariableName(node: Node): String = {
    node(TypeVariableDelta.TypeVariableName).asInstanceOf[String]
  }

  override def getType(compilation: Compilation, builder: ConstraintBuilder, path: NodeLike, parentScope: Scope): Type = {
    builder.typeVariable() //TODO fix mock implementation
  }

  override def shape: NodeShape = Shape
}
