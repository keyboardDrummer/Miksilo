package transformations.bytecode.types

import core.bigrammar.grammars.Keyword
import core.bigrammar.BiGrammar
import core.particles.Language
import core.particles.grammars.LanguageGrammars
import core.particles.node.{Node, NodeClass}

object ByteTypeC extends TypeInstance
{
  object ByteTypeKey extends NodeClass
  override val key = ByteTypeKey
  val me = new Node(ByteTypeKey)

  override def getSuperTypes(_type: Node, state: Language): Seq[Node] = ???

  override def getJavaGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    "byte" ~> value(me)
  }

  override def getByteCodeGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    new Keyword("B",false) ~> value(me)
  }

  override def description: String = "Adds the byte type."
}
