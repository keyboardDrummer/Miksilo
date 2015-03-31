package transformations.types

import core.particles.ParticleWithGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.node.Key
import transformations.types.ObjectTypeC.ObjectTypeByteCodeGrammarInner

object TypeApplication extends ParticleWithGrammar {

  object TypeApplicationKey extends Key
  object TypeApplicationFunc
  object TypeApplicationArgument

  object TypeArgumentGrammar

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    grammars.create(TypeArgumentGrammar)
    transformByteCodeGrammars(grammars)
    transformJavaGrammars(grammars)
  }

  def transformJavaGrammars(grammars: GrammarCatalogue): Unit = {
    val typeArgumentGrammar = grammars.find(TypeArgumentGrammar)
    val typeGrammar = grammars.find(TypeSkeleton.JavaTypeGrammar)
    val objectInner = grammars.find(ObjectTypeC.ObjectTypeJavaGrammar)
    typeGrammar.addOption(objectInner.inner ~ ("<" ~> typeArgumentGrammar <~ ">") ^^
      parseMap(TypeApplicationKey, TypeApplicationFunc, TypeApplicationArgument))
  }

  def transformByteCodeGrammars(grammars: GrammarCatalogue): Unit = {
    val typeArgumentGrammar = grammars.find(TypeArgumentGrammar)
    val objectInner = grammars.find(ObjectTypeByteCodeGrammarInner)
    objectInner.addOption(objectInner.inner ~ ("<" ~> typeArgumentGrammar <~ ">") ^^
      parseMap(TypeApplicationKey, TypeApplicationFunc, TypeApplicationArgument))
  }

  override def description: String = "Adds application of generic types"
}
