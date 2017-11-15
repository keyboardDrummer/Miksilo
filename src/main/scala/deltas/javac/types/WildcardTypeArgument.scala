package deltas.javac.types

import core.particles.{DeltaWithGrammar, Language}
import core.particles.grammars.LanguageGrammars
import core.particles.node.{Node, NodeClass}

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

