package deltas.javac.types

import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{NodeClass, NodeField}
import core.deltas.{DeltaWithGrammar, Language}
import deltas.bytecode.types.TypeSkeleton

object ExtendsTypeArgument extends DeltaWithGrammar {

  object ExtendsKey extends NodeClass
  object ExtendsBody extends NodeField
  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val byteCodeArgumentGrammar = find(TypeApplication.ByteCodeTypeArgumentGrammar)
    byteCodeArgumentGrammar.addOption(("+" ~~> byteCodeArgumentGrammar.as(ExtendsBody)).asNode(ExtendsKey))

    val javaTypeGrammar = find(TypeSkeleton.JavaTypeGrammar)
    val javaArgumentGrammar = find(TypeApplication.JavaTypeArgumentGrammar)
    javaArgumentGrammar.addOption(("?" ~~> "extends" ~~> javaTypeGrammar.as(ExtendsBody)).asNode(ExtendsKey))
  }

  override def description: String = "Adds the 'extends' type function. Example: 'T extends U'."
}
