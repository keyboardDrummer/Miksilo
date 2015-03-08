package transformations.types

import core.transformation.grammars.GrammarCatalogue
import core.transformation.{MetaObject, TransformationState}

object LongTypeC extends TypeInstance {

  override val key: AnyRef = LongTypeKey

  override def getSuperTypes(_type: MetaObject, state: TransformationState): Seq[MetaObject] = ???

  override def getByteCodeString(_type: MetaObject, state: TransformationState): String = "J"

  override def getStackSize: Int = 2

  override def getJavaGrammar(grammars: GrammarCatalogue) = {
    "long" ~> produce(longType)
  }

  def longType = new MetaObject(LongTypeKey)

  object LongTypeKey

  override def description: String = "Defines the long type."
}
