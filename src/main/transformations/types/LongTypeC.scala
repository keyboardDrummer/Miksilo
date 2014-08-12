package transformations.types

import core.transformation.grammars.GrammarCatalogue
import core.transformation.{MetaObject, TransformationState}

object LongTypeC extends TypeInstance {

  override val key: AnyRef = LongTypeKey

  override def getSuperTypes(_type: MetaObject, state: TransformationState): Seq[MetaObject] = ???

  override def getByteCodeString(_type: MetaObject, state: TransformationState): String = "J"

  override def getStackSize: Int = 2

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val typeGrammar = grammars.find(TypeC.TypeGrammar)
    typeGrammar.inner = typeGrammar.inner | ("long" ^^ (_ => ???))
  }

  object LongTypeKey

}
