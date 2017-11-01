package transformations.javac.types

import core.particles.grammars.GrammarCatalogue
import core.particles.node.{NodeClass, NodeField}
import core.particles.{DeltaWithGrammar, Language}
import transformations.bytecode.types.TypeSkeleton

object ExtendsTypeArgument extends DeltaWithGrammar {

  object ExtendsKey extends NodeClass
  object ExtendsBody extends NodeField
  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    import grammars._
    val byteCodeArgumentGrammar = find(TypeApplication.ByteCodeTypeArgumentGrammar)
    byteCodeArgumentGrammar.addOption(("+" ~~> byteCodeArgumentGrammar.as(ExtendsBody)).asNode(ExtendsKey))

    val javaTypeGrammar = find(TypeSkeleton.JavaTypeGrammar)
    val javaArgumentGrammar = find(TypeApplication.JavaTypeArgumentGrammar)
    javaArgumentGrammar.addOption(("?" ~~> "extends" ~~> javaTypeGrammar.as(ExtendsBody)).asNode(ExtendsKey))
  }

  override def description: String = "Adds the 'extends' type function. Example: 'T extends U'."
}
