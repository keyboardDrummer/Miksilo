package deltas.bytecode.types

import core.bigrammar.BiGrammar
import core.bigrammar.grammars.Keyword
import core.deltas.grammars.LanguageGrammars
import core.language.Compilation
import core.language.node.{Node, NodeLike, NodeShape}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.{PrimitiveType, Type}

object IntTypeDelta extends ByteCodeTypeInstance with HasStackTypeDelta {

  override val shape = Shape

  override def getSuperTypes(_type: Node): Seq[Node] = Seq.empty //TODO extend. long ?

  override def getByteCodeGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    new Keyword("I", false) ~> value(intType)
  }

  override def getJavaGrammar(grammars: LanguageGrammars) = {
    import grammars._
    "int" ~> value(intType)
  }

  val intType = new Node(Shape)

  override def getStackSize: Int = 1

  object Shape extends NodeShape

  override def description: String = "Defines the integer type."

  val constraintType = PrimitiveType("Int")
  override def getType(compilation: Compilation, builder: ConstraintBuilder, _type: NodeLike, parentScope: Scope): Type = constraintType

  override def constraintName = constraintType.name

  override def toConstraintType(_type: Type) = intType
}
