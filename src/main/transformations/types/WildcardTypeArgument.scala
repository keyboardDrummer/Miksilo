package transformations.types

import core.particles.ParticleWithGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.node.Node

object WildcardTypeArgument extends ParticleWithGrammar {

  object WildcardArgumentKey
  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val argumentGrammar = grammars.find(TypeApplication.TypeArgumentGrammar)
    argumentGrammar.addOption("*" ~> produce(new Node(WildcardArgumentKey)))
  }

  override def description: String = "Adds the wildcard type argument '*'."
}
