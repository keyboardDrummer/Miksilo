package transformations.types

import core.particles.grammars.GrammarCatalogue
import core.particles.CompilationState
import core.particles.node.MetaObject

object IntTypeC extends TypeInstance {

  override val key: AnyRef = IntTypeKey

  override def getSuperTypes(_type: MetaObject, state: CompilationState): Seq[MetaObject] = Seq.empty //TODO extend. long ?

  override def getByteCodeString(_type: MetaObject, state: CompilationState): String = "I"

  override def getJavaGrammar(grammars: GrammarCatalogue) = {
    "int" ~> produce(IntTypeC.intType)
  }

  def intType = new MetaObject(IntTypeKey)

  override def getStackSize: Int = 1

  object IntTypeKey

  override def description: String = "Defines the integer type."
}
