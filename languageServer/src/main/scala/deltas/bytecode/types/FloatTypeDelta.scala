package deltas.bytecode.types

import core.bigrammar.BiGrammar
import core.bigrammar.grammars.Keyword
import core.deltas.grammars.LanguageGrammars
import core.language.Compilation
import core.language.node.{Node, NodeLike, NodeShape}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.{PrimitiveType, Type}

object FloatTypeDelta extends ByteCodeTypeInstance
{
  object Shape extends NodeShape
  override val shape = Shape
  val floatType = new Node(Shape)

  override def getSuperTypes(_type: Node): Seq[Node] = ???

  override def getJavaGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    "float" ~> value(floatType)
  }

  override def getByteCodeGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    new Keyword("F",false) ~> value(floatType)
  }

  override def description: String = "Adds the float type."

  val constraintType = PrimitiveType("Float")
  override def getType(compilation: Compilation, builder: ConstraintBuilder, _type: NodeLike, parentScope: Scope): Type = constraintType
  override def constraintName = constraintType.name

  override def fromConstraintType(compilation: Compilation, _type: Type) = floatType
}
