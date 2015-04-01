package transformations.types

import core.particles.ParticleWithGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.node.Key
import transformations.types.ObjectTypeC.ObjectTypeByteCodeGrammarInner

object TypeApplication extends ParticleWithGrammar {

  object TypeApplicationKey extends Key
  object TypeApplicationFunc
  object TypeApplicationArgument


  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    transformByteCodeGrammars(grammars)
    transformJavaGrammars(grammars)
  }

  object JavaTypeArgumentGrammar
  def transformJavaGrammars(grammars: GrammarCatalogue): Unit = {
    val typeArgumentGrammar = grammars.create(JavaTypeArgumentGrammar)
    val typeGrammar = grammars.find(TypeSkeleton.JavaTypeGrammar)
    val objectInner = grammars.find(ObjectTypeC.ObjectTypeJavaGrammar)
    typeGrammar.addOption(objectInner.inner ~ ("<" ~> typeArgumentGrammar.someSeparated(",") <~ ">") ^^
      parseMap(TypeApplicationKey, TypeApplicationFunc, TypeApplicationArgument))
  }

  object ByteCodeTypeArgumentGrammar
  def transformByteCodeGrammars(grammars: GrammarCatalogue): Unit = {
    val typeArgumentGrammar = grammars.create(ByteCodeTypeArgumentGrammar)
    val objectInner = grammars.find(ObjectTypeByteCodeGrammarInner)
    objectInner.addOption(objectInner.inner ~ ("<" ~> typeArgumentGrammar.some <~ ">") ^^
      parseMap(TypeApplicationKey, TypeApplicationFunc, TypeApplicationArgument))
  }

  override def description: String = "Adds application of generic types"
}
