package transformations.javac.types

import core.particles.grammars.GrammarCatalogue
import core.particles.node.{NodeClass, NodeField}
import core.particles.{DeltaWithGrammar, Language}
import transformations.bytecode.types.TypeSkeleton

object SuperTypeArgument extends DeltaWithGrammar {

  object SuperKey extends NodeClass
  object SuperBody extends NodeField
  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    val byteCodeArgumentGrammar = grammars.find(TypeApplication.ByteCodeTypeArgumentGrammar)
    byteCodeArgumentGrammar.addOption(("-" ~~> byteCodeArgumentGrammar).asNode(SuperKey, SuperBody))

    val javaTypeGrammar = grammars.find(TypeSkeleton.JavaTypeGrammar)
    val javaArgumentGrammar = grammars.find(TypeApplication.JavaTypeArgumentGrammar)
    javaArgumentGrammar.addOption(("?" ~~> "super" ~~> javaTypeGrammar).asNode(SuperKey, SuperBody))
  }

  override def description: String = "Adds the 'extends' type function. Example: 'T extends U'."
}
