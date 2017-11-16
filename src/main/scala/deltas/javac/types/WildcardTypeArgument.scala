package deltas.javac.types

import core.deltas.{DeltaWithGrammar, Language}
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeClass}

object WildcardTypeArgument extends DeltaWithGrammar {

  object WildcardArgumentKey extends NodeClass
  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val byteCodeArgumentGrammar = find(TypeApplication.ByteCodeTypeArgumentGrammar)
    byteCodeArgumentGrammar.addOption("*" ~> value(new Node(WildcardArgumentKey)))

    val javaArgumentGrammar = find(TypeApplication.JavaTypeArgumentGrammar)
    javaArgumentGrammar.addOption("?" ~> value(new Node(WildcardArgumentKey)))
  }

  override def description: String = "Adds the wildcard type argument '*'."
}

