package transformations.javac.types

import core.particles.DeltaWithGrammar
import core.particles.grammars.GrammarCatalogue
import transformations.bytecode.types.TypeSkeleton

object SuperTypeArgument extends DeltaWithGrammar {

  object SuperKey
  object SuperBody
  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val byteCodeArgumentGrammar = grammars.find(TypeApplication.ByteCodeTypeArgumentGrammar)
    byteCodeArgumentGrammar.addOption("-" ~~> byteCodeArgumentGrammar ^^ parseMap(SuperKey, SuperBody))

    val javaTypeGrammar = grammars.find(TypeSkeleton.JavaTypeGrammar)
    val javaArgumentGrammar = grammars.find(TypeApplication.JavaTypeArgumentGrammar)
    javaArgumentGrammar.addOption("?" ~~> "super" ~~> javaTypeGrammar ^^
      parseMap(SuperKey, SuperBody))
  }

  override def description: String = "Adds the 'extends' type function. Example: 'T extends U'."
}
