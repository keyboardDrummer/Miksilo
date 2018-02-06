package deltas.javac.types

import core.bigrammar.BiGrammar
import core.bigrammar.grammars.Keyword
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeField, NodeLike, NodeShape}
import core.deltas.{Compilation, DeltaWithGrammar}
import core.language.Language
import core.nabl.ConstraintBuilder
import core.nabl.scopes.objects.Scope
import core.nabl.types.objects.Type
import deltas.bytecode.types.{HasType, TypeSkeleton}

object TypeVariable extends DeltaWithGrammar with HasType {

  override def description: String = "Adds references to type variables."

  object Shape extends NodeShape
  object TypeVariableName extends NodeField
  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    transformByteCodeGrammar(grammars)
    transformJavaGrammar(grammars)
  }

  def transformJavaGrammar(grammars: LanguageGrammars): Unit = {
    import grammars._
    val typeGrammar = find(TypeSkeleton.JavaTypeGrammar)
    val variableGrammar: BiGrammar = identifier.as(TypeVariableName).asNode(Shape)
    typeGrammar.addOption(variableGrammar)
  }

  def transformByteCodeGrammar(grammars: LanguageGrammars): Unit = {
    import grammars._
    val byteCodeType = find(TypeSkeleton.ByteCodeTypeGrammar)
    byteCodeType.addOption(Keyword("T", false) ~> identifier.as(TypeVariableName) ~< ";" asNode Shape)
  }

  def getTypeVariableName(node: Node): String = {
    node(TypeVariable.TypeVariableName).asInstanceOf[String]
  }

  override def getType(compilation: Compilation, builder: ConstraintBuilder, path: NodeLike, parentScope: Scope): Type = {
    builder.typeVariable() //TODO fix mock implementation
  }

  override def shape: NodeShape = Shape
}
