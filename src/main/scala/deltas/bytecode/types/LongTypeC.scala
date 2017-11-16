package deltas.bytecode.types

import core.bigrammar.grammars.Keyword
import core.bigrammar.BiGrammar
import core.deltas.Language
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeClass}

object LongTypeC extends TypeInstance with StackType {

  override val key = LongTypeKey

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

  object LongTypeKey extends NodeClass

  override def description: String = "Defines the long type."
}
