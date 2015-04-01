package transformations.types

import core.particles.ParticleWithGrammar
import core.particles.grammars.GrammarCatalogue

object ConcreteTypeArgument extends ParticleWithGrammar {
  object ConcreteArgumentKey
  object ConcreteArgumentType
  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val typeGrammar = grammars.find(TypeSkeleton.ByteCodeTypeGrammar)
    val argumentGrammar = grammars.find(TypeApplication.ByteCodeTypeArgumentGrammar)
    argumentGrammar.addOption(typeGrammar ^^ parseMap(ConcreteArgumentKey, ConcreteArgumentType))

    val javaTypeGrammar = grammars.find(TypeSkeleton.JavaTypeGrammar)
    val javaArgumentGrammar = grammars.find(TypeApplication.JavaTypeArgumentGrammar)
    javaArgumentGrammar.addOption(javaTypeGrammar ^^ parseMap(ConcreteArgumentKey, ConcreteArgumentType))
  }

  override def description: String = "Enables using concrete types as type arguments."
}
