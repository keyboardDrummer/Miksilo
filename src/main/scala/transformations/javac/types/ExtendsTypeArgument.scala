package transformations.javac.types

import core.particles.{Language, DeltaWithGrammar}
import core.particles.grammars.GrammarCatalogue
import core.particles.node.Key
import transformations.bytecode.types.TypeSkeleton

object ExtendsTypeArgument extends DeltaWithGrammar {

  object ExtendsKey extends Key
  object ExtendsBody extends Key
  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    val byteCodeArgumentGrammar = grammars.find(TypeApplication.ByteCodeTypeArgumentGrammar)
    byteCodeArgumentGrammar.addOption(("+" ~~> byteCodeArgumentGrammar).asNode(ExtendsKey, ExtendsBody))

    val javaTypeGrammar = grammars.find(TypeSkeleton.JavaTypeGrammar)
    val javaArgumentGrammar = grammars.find(TypeApplication.JavaTypeArgumentGrammar)
    javaArgumentGrammar.addOption(("?" ~~> "extends" ~~> javaTypeGrammar).asNode(ExtendsKey, ExtendsBody))
  }

  override def description: String = "Adds the 'extends' type function. Example: 'T extends U'."
}
