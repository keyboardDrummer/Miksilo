package deltas.javac.types

import core.bigrammar.BiGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{GrammarKey, NodeField, NodeShape}
import core.deltas.DeltaWithGrammar
import core.language.Language
import deltas.bytecode.types.{QualifiedObjectTypeDelta, TypeSkeleton, UnqualifiedObjectTypeDelta}

object TypeApplication extends DeltaWithGrammar {

  override def description: String = "Adds application of generic types"

  object Shape extends NodeShape
  object TypeApplicationFunc extends NodeField
  object TypeApplicationArgument extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    transformByteCodeGrammars(grammars)
    transformJavaGrammars(grammars)
  }

  object JavaTypeArgumentGrammar extends GrammarKey
  def transformJavaGrammars(grammars: LanguageGrammars): Unit = {
    import grammars._
    val typeArgumentGrammar = create(JavaTypeArgumentGrammar)
    val typeGrammar = find(TypeSkeleton.JavaTypeGrammar)
    val objectInner = find(UnqualifiedObjectTypeDelta.AnyObjectTypeGrammar)
    typeArgumentGrammar.addOption(typeGrammar)
    val typeApplication: BiGrammar = (objectInner.inner.as(TypeApplicationFunc) ~ ("<" ~> typeArgumentGrammar.someSeparated(",").as(TypeApplicationArgument) ~< ">")).
      asNode(Shape)
    typeGrammar.addOption(typeApplication)
  }

  object ByteCodeTypeArgumentGrammar extends GrammarKey
  def transformByteCodeGrammars(grammars: LanguageGrammars): Unit = {
    import grammars._
    val typeArgumentGrammar = create(ByteCodeTypeArgumentGrammar)
    val objectInner = find(QualifiedObjectTypeDelta.ByteCodeGrammarInner)
    objectInner.addOption((objectInner.inner.as(TypeApplicationFunc) ~ ("<" ~> typeArgumentGrammar.some.as(TypeApplicationArgument) ~< ">")).
      asNode(Shape))

    val typeGrammar = find(TypeSkeleton.ByteCodeTypeGrammar)
    val argumentGrammar = find(TypeApplication.ByteCodeTypeArgumentGrammar)
    argumentGrammar.addOption(typeGrammar)
  }
}

