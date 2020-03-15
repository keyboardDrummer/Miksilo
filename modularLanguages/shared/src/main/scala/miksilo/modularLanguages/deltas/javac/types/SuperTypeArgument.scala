package miksilo.modularLanguages.deltas.javac.types

import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.node.{NodeField, NodeShape}
import miksilo.modularLanguages.core.deltas.{Contract, DeltaWithGrammar}
import miksilo.languageServer.core.language.Language
import miksilo.modularLanguages.deltas.bytecode.types.TypeSkeleton

object SuperTypeArgument extends DeltaWithGrammar {

  object SuperKey extends NodeShape
  object SuperBody extends NodeField
  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val byteCodeArgumentGrammar = find(TypeApplicationDelta.ByteCodeTypeArgumentGrammar)
    byteCodeArgumentGrammar.addAlternative(("-" ~~> byteCodeArgumentGrammar.as(SuperBody)).asNode(SuperKey))

    val javaTypeGrammar = find(TypeSkeleton.JavaTypeGrammar)
    val javaArgumentGrammar = find(TypeApplicationDelta.JavaTypeArgumentGrammar)
    javaArgumentGrammar.addAlternative(("?" ~~> "super" ~~> javaTypeGrammar.as(SuperBody)).asNode(SuperKey))
  }

  override def description: String = "Adds the 'extends' type function. Example: 'T extends U'."

  override def dependencies: Set[Contract] = Set(TypeApplicationDelta)
}
