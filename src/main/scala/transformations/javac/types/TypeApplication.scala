package transformations.javac.types

import core.bigrammar.BiGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{NodeClass, NodeField}
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

  object JavaTypeArgumentGrammar
  def transformJavaGrammars(grammars: GrammarCatalogue): Unit = {
    val typeArgumentGrammar = grammars.create(JavaTypeArgumentGrammar)
    val typeGrammar = grammars.find(TypeSkeleton.JavaTypeGrammar)
    val objectInner = grammars.find(ObjectTypeDelta.ObjectTypeJavaGrammar)
    typeArgumentGrammar.addOption(typeGrammar)
    val typeApplication: BiGrammar = (objectInner.inner ~ ("<" ~> typeArgumentGrammar.someSeparated(",") ~< ">")).
      asNode(TypeApplicationKey, TypeApplicationFunc, TypeApplicationArgument)
    typeGrammar.addOption(typeApplication)
  }

  object ByteCodeTypeArgumentGrammar
  def transformByteCodeGrammars(grammars: GrammarCatalogue): Unit = {
    val typeArgumentGrammar = grammars.create(ByteCodeTypeArgumentGrammar)
    val objectInner = grammars.find(ObjectTypeByteCodeGrammarInner)
    objectInner.addOption((objectInner.inner ~ ("<" ~> typeArgumentGrammar.some ~< ">")).
      asNode(TypeApplicationKey, TypeApplicationFunc, TypeApplicationArgument))

    val typeGrammar = grammars.find(TypeSkeleton.ByteCodeTypeGrammar)
    val argumentGrammar = grammars.find(TypeApplication.ByteCodeTypeArgumentGrammar)
    argumentGrammar.addOption(typeGrammar)
  }

  override def description: String = "Adds application of generic types"
}

