package miksilo.modularLanguages.deltas.bytecode.types

import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.bigrammar.grammars.Keyword
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node.{Node, NodeLike, NodeShape}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.{PrimitiveType, Type}

object VoidTypeDelta extends ByteCodeTypeInstance with HasStackTypeDelta {

  override val shape = Shape

  override def getSuperTypes(_type: Node): Seq[Node] = ???

  override def getByteCodeGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    Keyword("V", false) ~> value(voidType)
  }

  override def getStackSize: Int = 0

  override def getJavaGrammar(grammars: LanguageGrammars) = {
    import grammars._
    "void" ~> value(voidType)
  }

  def voidType = new Node(Shape)

  object Shape extends NodeShape

  override def description: String = "Defines the void type."

  val constraintType = PrimitiveType("Void")
  override def getType(compilation: Compilation, builder: ConstraintBuilder, _type: NodeLike, parentScope: Scope): Type = constraintType

  override def constraintName = constraintType.name

  override def fromConstraintType(_type: Type) = voidType
}
