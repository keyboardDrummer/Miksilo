package deltas.bytecode.types

import core.deltas.grammars.LanguageGrammars
import core.language.Compilation
import core.language.node.{Node, NodeLike, NodeShape}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.{PrimitiveType, Type}

object ByteTypeDelta extends ByteCodeTypeInstance {

  override def description: String = "Adds the byte type."

  object Shape extends NodeShape
  override val shape = Shape
  val me = new Node(Shape)

  override def getSuperTypes(_type: Node): Seq[Node] = ???

  override def getJavaGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    "byte" ~> value(me)
  }

  override def getByteCodeGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    Keyword("B", false) ~> value(me)
  }

  val constraintType = PrimitiveType("Byte")
  override def getType(compilation: Compilation, builder: ConstraintBuilder, _type: NodeLike, parentScope: Scope): Type =
    constraintType

  override def constraintName = constraintType.name

  override def fromConstraintType(_type: Type) = me
}
