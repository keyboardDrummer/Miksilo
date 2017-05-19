package transformations.javac.types

import core.particles.DeltaWithGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.node.Key
import transformations.bytecode.types.TypeSkeleton

import scala.swing.event.Key

object SuperTypeArgument extends DeltaWithGrammar {

  object SuperKey extends Key
  object SuperBody extends Key
  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val byteCodeArgumentGrammar = grammars.find(TypeApplication.ByteCodeTypeArgumentGrammar)
    byteCodeArgumentGrammar.addOption(("-" ~~> byteCodeArgumentGrammar).asNode(SuperKey, SuperBody))

    val javaTypeGrammar = grammars.find(TypeSkeleton.JavaTypeGrammar)
    val javaArgumentGrammar = grammars.find(TypeApplication.JavaTypeArgumentGrammar)
    javaArgumentGrammar.addOption(("?" ~~> "super" ~~> javaTypeGrammar).asNode(SuperKey, SuperBody))
  }

  override def description: String = "Adds the 'extends' type function. Example: 'T extends U'."
}
