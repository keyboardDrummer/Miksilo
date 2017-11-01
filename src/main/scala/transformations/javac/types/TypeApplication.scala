package transformations.javac.types

import core.bigrammar.BiGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{GrammarKey, NodeClass, NodeField}
import core.particles.{DeltaWithGrammar, Language}
import transformations.bytecode.types.ObjectTypeDelta.ObjectTypeByteCodeGrammarInner
import transformations.bytecode.types.{ObjectTypeDelta, TypeSkeleton}

object TypeApplication extends DeltaWithGrammar {

  object TypeApplicationKey extends NodeClass
  object TypeApplicationFunc extends NodeField
  object TypeApplicationArgument extends NodeField

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    transformByteCodeGrammars(grammars)
    transformJavaGrammars(grammars)
  }

  object JavaTypeArgumentGrammar extends GrammarKey
  def transformJavaGrammars(grammars: GrammarCatalogue): Unit = {
    import grammars._
    val typeArgumentGrammar = create(JavaTypeArgumentGrammar)
    val typeGrammar = find(TypeSkeleton.JavaTypeGrammar)
    val objectInner = find(ObjectTypeDelta.ObjectTypeJavaGrammar)
    typeArgumentGrammar.addOption(typeGrammar)
    val typeApplication: BiGrammar = (objectInner.inner.as(TypeApplicationFunc) ~ ("<" ~> typeArgumentGrammar.someSeparated(",").as(TypeApplicationArgument) ~< ">")).
      asNode(TypeApplicationKey)
    typeGrammar.addOption(typeApplication)
  }

  object ByteCodeTypeArgumentGrammar extends GrammarKey
  def transformByteCodeGrammars(grammars: GrammarCatalogue): Unit = {
    import grammars._
    val typeArgumentGrammar = create(ByteCodeTypeArgumentGrammar)
    val objectInner = find(ObjectTypeByteCodeGrammarInner)
    objectInner.addOption((objectInner.inner.as(TypeApplicationFunc) ~ ("<" ~> typeArgumentGrammar.some.as(TypeApplicationArgument) ~< ">")).
      asNode(TypeApplicationKey))

    val typeGrammar = find(TypeSkeleton.ByteCodeTypeGrammar)
    val argumentGrammar = find(TypeApplication.ByteCodeTypeArgumentGrammar)
    argumentGrammar.addOption(typeGrammar)
  }

  override def description: String = "Adds application of generic types"
}

