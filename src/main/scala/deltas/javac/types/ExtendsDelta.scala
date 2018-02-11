package deltas.javac.types

import core.deltas.grammars.LanguageGrammars
import core.language.node.{NodeField, NodeLike, NodeShape}
import core.deltas.{DeltaWithGrammar, HasShape}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.bytecode.types.{HasType, TypeSkeleton}

object ExtendsDelta extends DeltaWithGrammar with HasShape with HasType {

  override def description: String = "Adds the 'extends' type function. Example: 'T extends U'."

  object Shape extends NodeShape
  object ExtendsBody extends NodeField
  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val byteCodeArgumentGrammar = find(TypeApplicationDelta.ByteCodeTypeArgumentGrammar)
    byteCodeArgumentGrammar.addOption(("+" ~~> byteCodeArgumentGrammar.as(ExtendsBody)).asNode(Shape))

    val javaTypeGrammar = find(TypeSkeleton.JavaTypeGrammar)
    val javaArgumentGrammar = find(TypeApplicationDelta.JavaTypeArgumentGrammar)
    javaArgumentGrammar.addOption(("?" ~~> "extends" ~~> javaTypeGrammar.as(ExtendsBody)).asNode(Shape))
  }

  val shape: NodeShape = Shape

  override def getType(compilation: Compilation, builder: ConstraintBuilder, path: NodeLike, parentScope: Scope): Type = {
    builder.typeVariable() //TODO add generics.
  }
}
