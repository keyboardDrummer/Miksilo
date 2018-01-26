package deltas.bytecode.types

import core.bigrammar.BiGrammar
import core.bigrammar.grammars.Keyword
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeShape}
import core.language.Language

object FloatTypeC extends TypeInstance
{
  object FloatTypeKey extends NodeShape
  override val key = FloatTypeKey
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
}
