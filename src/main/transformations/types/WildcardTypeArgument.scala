package transformations.types

import core.bigrammar.BiGrammar
import core.particles.ParticleWithGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.node.Node

object WildcardTypeArgument extends ParticleWithGrammar {

  object WildcardArgumentKey
  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val byteCodeArgumentGrammar = grammars.find(TypeApplication.ByteCodeTypeArgumentGrammar)
    val wildcardGrammar: BiGrammar = "*" ~> produce(new Node(WildcardArgumentKey))
    byteCodeArgumentGrammar.addOption(wildcardGrammar)

    val javaArgumentGrammar = grammars.find(TypeApplication.JavaTypeArgumentGrammar)
    javaArgumentGrammar.addOption(wildcardGrammar)
  }

  override def description: String = "Adds the wildcard type argument '*'."
}

