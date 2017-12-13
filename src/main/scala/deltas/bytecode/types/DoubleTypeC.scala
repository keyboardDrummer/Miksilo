package deltas.bytecode.types

import core.bigrammar.grammars.Keyword
import core.bigrammar.{BiGrammar}
import core.deltas.Language
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeShape}

object DoubleTypeC extends TypeInstance with StackType {

  override val key = DoubleTypeKey

  override def getSuperTypes(_type: Node, state: Language): Seq[Node] = ???

  override def getByteCodeGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    new Keyword("D",false) ~> value(doubleType)
  }

  override def getStackSize: Int = 2

  override def getJavaGrammar(grammars: LanguageGrammars) = {
    import grammars._
    "double" ~> value(doubleType)
  }

  val doubleType = new Node(key)

  object DoubleTypeKey extends NodeShape

  override def description: String = "Defines the double type."
}
