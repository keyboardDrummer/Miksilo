package transformations.types

import core.particles.grammars.GrammarCatalogue
import core.particles.{CompilationState, MetaObject}

object DoubleTypeC extends TypeInstance {

  override val key: AnyRef = DoubleTypeKey

  override def getSuperTypes(_type: MetaObject, state: CompilationState): Seq[MetaObject] = ???

  override def getByteCodeString(_type: MetaObject, state: CompilationState): String = "D"

  override def getStackSize: Int = 2

  override def getJavaGrammar(grammars: GrammarCatalogue) = {
    "double" ~> produce(doubleType)
  }

  val doubleType = new MetaObject(key)

  object DoubleTypeKey

  override def description: String = "Defines the double type."
}
