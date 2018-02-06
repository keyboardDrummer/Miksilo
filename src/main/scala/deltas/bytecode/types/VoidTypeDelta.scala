package deltas.bytecode.types

import core.bigrammar.BiGrammar
import core.bigrammar.grammars.Keyword
import core.deltas.Compilation
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeLike, NodeShape}
import core.language.Language
import core.nabl.ConstraintBuilder
import core.nabl.scopes.objects.Scope
import core.nabl.types.objects.{PrimitiveType, Type}

object VoidTypeDelta extends TypeInstance with StackType {

  override val shape = VoidTypeKey

  override def getSuperTypes(_type: Node, state: Language): Seq[Node] = ???

  override def getByteCodeGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    Keyword("V", false) ~> value(voidType)
  }

  override def getStackSize: Int = 0

  override def getJavaGrammar(grammars: LanguageGrammars) = {
    import grammars._
    "void" ~> value(voidType)
  }

  def voidType = new Node(VoidTypeKey)

  object VoidTypeKey extends NodeShape

  override def description: String = "Defines the void type."

  val constraintType = PrimitiveType("Void")
  override def getType(compilation: Compilation, builder: ConstraintBuilder, _type: NodeLike, parentScope: Scope): Type = constraintType
}
