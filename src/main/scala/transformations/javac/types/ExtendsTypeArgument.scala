package transformations.javac.types

import core.particles.grammars.GrammarCatalogue
import core.particles.node.{NodeClass, NodeField}
import core.particles.{DeltaWithGrammar, Language}
import transformations.bytecode.types.TypeSkeleton

object ExtendsTypeArgument extends DeltaWithGrammar {

  object ExtendsKey extends NodeClass
  object ExtendsBody extends NodeField
  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    val byteCodeArgumentGrammar = grammars.find(TypeApplication.ByteCodeTypeArgumentGrammar)
    byteCodeArgumentGrammar.addOption(("+" ~~> byteCodeArgumentGrammar).asNode(ExtendsKey, ExtendsBody))

    val javaTypeGrammar = grammars.find(TypeSkeleton.JavaTypeGrammar)
    val javaArgumentGrammar = grammars.find(TypeApplication.JavaTypeArgumentGrammar)
    javaArgumentGrammar.addOption(("?" ~~> "extends" ~~> javaTypeGrammar).asNode(ExtendsKey, ExtendsBody))
  }

  override def description: String = "Adds the 'extends' type function. Example: 'T extends U'."
}
