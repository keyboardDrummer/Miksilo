package deltas.bytecode.types

import core.bigrammar.grammars.Keyword
import core.bigrammar.BiGrammar
import core.deltas.Language
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeShape}

object VoidTypeC extends TypeInstance with StackType {

  override val key = VoidTypeKey

  override def getSuperTypes(_type: Node, state: Language): Seq[Node] = ???

  override def getByteCodeGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    Keyword("V", false) ~> value(voidType)
  }

  override def getStackSize: Int = 0

  override def getJavaGrammar(grammars: LanguageGrammars) = {
    import grammars._
    "void" ~> value(voidType)
  }

  def voidType = new Node(VoidTypeKey)

  object VoidTypeKey extends NodeShape

  override def description: String = "Defines the void type."
}
