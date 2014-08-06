package transformations.javac.types

import core.transformation.grammars.GrammarCatalogue
import core.transformation.{MetaObject, TransformationState}

object DoubleTypeC extends TypeInstance {

  object DoubleTypeKey

  override val key: AnyRef = DoubleTypeKey

  override def getSuperTypes(_type: MetaObject, state: TransformationState): Seq[MetaObject] = ???

  override def getByteCodeString(_type: MetaObject, state: TransformationState): String = "D"

  override def getStackSize: Int = 2

  override def transformGrammars(grammars: GrammarCatalogue): Unit = ???
}
