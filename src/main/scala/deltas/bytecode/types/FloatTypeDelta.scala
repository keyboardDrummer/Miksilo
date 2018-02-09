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

object FloatTypeDelta extends TypeInstance
{
  object FloatTypeKey extends NodeShape
  override val shape = FloatTypeKey
  val floatType = new Node(FloatTypeKey)

  override def getSuperTypes(_type: Node, state: Language): Seq[Node] = ???

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
}
