package transformations.javac.types

import core.particles.DeltaWithGrammar
import core.particles.grammars.GrammarCatalogue
import transformations.bytecode.types.TypeSkeleton

object ExtendsTypeArgument extends DeltaWithGrammar {

  object ExtendsKey
  object ExtendsBody
  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val byteCodeArgumentGrammar = grammars.find(TypeApplication.ByteCodeTypeArgumentGrammar)
    byteCodeArgumentGrammar.addOption("+" ~~> byteCodeArgumentGrammar ^^ parseMap(ExtendsKey, ExtendsBody))

    val javaTypeGrammar = grammars.find(TypeSkeleton.JavaTypeGrammar)
    val javaArgumentGrammar = grammars.find(TypeApplication.JavaTypeArgumentGrammar)
    javaArgumentGrammar.addOption("?" ~~> "extends" ~~> javaTypeGrammar ^^
      parseMap(ExtendsKey, ExtendsBody))
  }

  override def description: String = "Adds the 'extends' type function. Example: 'T extends U'."
}
