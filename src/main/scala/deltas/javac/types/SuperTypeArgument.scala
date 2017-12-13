package deltas.javac.types

import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{NodeShape, NodeField}
import core.deltas.{DeltaWithGrammar, Language}
import deltas.bytecode.types.TypeSkeleton

object SuperTypeArgument extends DeltaWithGrammar {

  object SuperKey extends NodeShape
  object SuperBody extends NodeField
  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val byteCodeArgumentGrammar = find(TypeApplication.ByteCodeTypeArgumentGrammar)
    byteCodeArgumentGrammar.addOption(("-" ~~> byteCodeArgumentGrammar.as(SuperBody)).asNode(SuperKey))

    val javaTypeGrammar = find(TypeSkeleton.JavaTypeGrammar)
    val javaArgumentGrammar = find(TypeApplication.JavaTypeArgumentGrammar)
    javaArgumentGrammar.addOption(("?" ~~> "super" ~~> javaTypeGrammar.as(SuperBody)).asNode(SuperKey))
  }

  override def description: String = "Adds the 'extends' type function. Example: 'T extends U'."
}
