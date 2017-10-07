package transformations.javac.types

import core.particles.{DeltaWithGrammar, Language}
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeClass}

object WildcardTypeArgument extends DeltaWithGrammar {

  object WildcardArgumentKey extends NodeClass
  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    val byteCodeArgumentGrammar = grammars.find(TypeApplication.ByteCodeTypeArgumentGrammar)
    byteCodeArgumentGrammar.addOption("*" ~> value(new Node(WildcardArgumentKey)))

    val javaArgumentGrammar = grammars.find(TypeApplication.JavaTypeArgumentGrammar)
    javaArgumentGrammar.addOption("?" ~> value(new Node(WildcardArgumentKey)))
  }

  override def description: String = "Adds the wildcard type argument '*'."
}

