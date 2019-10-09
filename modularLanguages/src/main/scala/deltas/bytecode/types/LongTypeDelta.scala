package deltas.bytecode.types

import core.deltas.grammars.LanguageGrammars
import core.language.Compilation
import core.language.node.{Node, NodeLike, NodeShape}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.{PrimitiveType, Type}

object LongTypeDelta extends ByteCodeTypeInstance with HasStackTypeDelta {

  override val shape = Shape

  override def getSuperTypes(_type: Node): Seq[Node] = Seq.empty

  override def getByteCodeGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    Keyword("J", false) ~> value(longType)
  }

  override def getStackSize: Int = 2

  override def getJavaGrammar(grammars: LanguageGrammars) = {
    import grammars._
    "long" ~> value(longType)
  }

  val longType = new Node(Shape)

  object Shape extends NodeShape

  override def description: String = "Defines the long type."

  val constraintType = PrimitiveType("Long")
  override def getType(compilation: Compilation, builder: ConstraintBuilder, _type: NodeLike, parentScope: Scope): Type =
    constraintType

  override def constraintName = constraintType.name

  override def fromConstraintType(_type: Type) = longType
}
