package transformations.types

import core.particles.grammars.GrammarCatalogue
import core.particles.{MetaObject, CompilationState}

object LongTypeC extends TypeInstance {

  override val key: AnyRef = LongTypeKey

  override def getSuperTypes(_type: MetaObject, state: CompilationState): Seq[MetaObject] = ???

  override def getByteCodeString(_type: MetaObject, state: CompilationState): String = "J"

  override def getStackSize: Int = 2

  override def getJavaGrammar(grammars: GrammarCatalogue) = {
    "long" ~> produce(longType)
  }

  def longType = new MetaObject(LongTypeKey)

  object LongTypeKey

  override def description: String = "Defines the long type."
}
