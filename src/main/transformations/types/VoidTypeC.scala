package transformations.types

import core.particles.grammars.GrammarCatalogue
import core.particles.{MetaObject, CompilationState}

object VoidTypeC extends TypeInstance {

  override val key: AnyRef = VoidTypeKey

  override def getSuperTypes(_type: MetaObject, state: CompilationState): Seq[MetaObject] = ???

  override def getByteCodeString(_type: MetaObject, state: CompilationState): String = "V"

  override def getStackSize: Int = 0

  override def getJavaGrammar(grammars: GrammarCatalogue) = {
    "void" ~> produce(voidType)
  }

  def voidType = new MetaObject(VoidTypeKey)

  object VoidTypeKey

  override def description: String = "Defines the void type."
}
