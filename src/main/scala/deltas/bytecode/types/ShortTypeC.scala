package deltas.bytecode.types

import core.bigrammar.grammars.Keyword
import core.bigrammar.BiGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeShape}
import core.language.Language

object ShortTypeC extends TypeInstance with StackType {

  override val key = ShortTypeKey

  override def getSuperTypes(_type: Node, state: Language): Seq[Node] = Seq.empty //TODO extend. long ?


  override def getByteCodeGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    new Keyword("S",false) ~> value(shortType)
  }

  override def getJavaGrammar(grammars: LanguageGrammars) = {
    import grammars._
    "short" ~> value(shortType)
  }

  def shortType = new Node(ShortTypeKey)

  override def getStackSize: Int = 1

  object ShortTypeKey extends NodeShape

  override def description: String = "Defines the short type."
}
