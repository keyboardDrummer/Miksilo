package deltas.bytecode.types

import core.bigrammar.{BiGrammar, WithMap}
import core.bigrammar.grammars.Keyword
import core.deltas.grammars.LanguageGrammars
import core.language.Compilation
import core.language.node.{Node, NodeLike, NodeShape}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.{PrimitiveType, Type}

object CharTypeDelta extends ByteCodeTypeInstance
{
  object Shape extends NodeShape
  override val shape = Shape
  val me = new Node(Shape)

  override def getSuperTypes(_type: Node): Seq[Node] = ???

  override def getJavaGrammar(grammars: LanguageGrammars): BiGrammar[WithMap[Node]] = {
    import grammars._
    Keyword("char") ~> value(me)
  }

  override def getByteCodeGrammar(grammars: LanguageGrammars): BiGrammar[WithMap[Node]] = {
    import grammars._
    Keyword("C", reserved = false) ~> value(me)
  }

  override def description: String = "Adds the char type."

  val constraintType = PrimitiveType("Char")
  override def getType(compilation: Compilation, builder: ConstraintBuilder, _type: NodeLike, parentScope: Scope): Type = constraintType

  override def constraintName = constraintType.name

  override def fromConstraintType(_type: Type) = me
}
