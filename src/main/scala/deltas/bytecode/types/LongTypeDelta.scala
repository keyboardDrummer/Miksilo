package deltas.bytecode.types

import core.bigrammar.BiGrammar
import core.bigrammar.grammars.Keyword
import core.deltas.Compilation
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeLike, NodeShape}
import core.language.Language
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.{PrimitiveType, Type}

object LongTypeDelta extends TypeInstance with StackType {

  override val shape = LongTypeKey

  override def getSuperTypes(_type: Node, state: Language): Seq[Node] = Seq.empty

  override def getByteCodeGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    new Keyword("J",false) ~> value(longType)
  }

  override def getStackSize: Int = 2

  override def getJavaGrammar(grammars: LanguageGrammars) = {
    import grammars._
    "long" ~> value(longType)
  }

  val longType = new Node(LongTypeKey)

  object LongTypeKey extends NodeShape

  override def description: String = "Defines the long type."

  val constraintType: Type = PrimitiveType("Long")
  override def getType(compilation: Compilation, builder: ConstraintBuilder, _type: NodeLike, parentScope: Scope): Type = constraintType
}
