package deltas.bytecode.types

import core.bigrammar.BiGrammar
import core.bigrammar.grammars.Keyword
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
    "byte" ~> valueGrammar(me)
  }

  override def getByteCodeGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    Keyword("B", false) ~> valueGrammar(me)
  }

  val constraintType = PrimitiveType("Byte")
  override def getType(compilation: Compilation, builder: ConstraintBuilder, _type: NodeLike, parentScope: Scope): Type =
    constraintType

  override def constraintName = constraintType.name

  override def fromConstraintType(_type: Type) = me
}
