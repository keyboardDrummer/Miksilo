package transformations.types

import core.particles.grammars.GrammarCatalogue
import core.particles.{CompilationState, MetaObject}

object BooleanTypeC extends TypeInstance {
  override val key: AnyRef = BooleanTypeKey

  override def getSuperTypes(_type: MetaObject, state: CompilationState): Seq[MetaObject] = Seq.empty

  override def getByteCodeString(_type: MetaObject, state: CompilationState): String = "Z"


  override def getStackType(_type: MetaObject, state: CompilationState): MetaObject = IntTypeC.intType

  override def getJavaGrammar(grammars: GrammarCatalogue) = {
    "boolean" ~> produce(booleanType)
  }

  def booleanType = new MetaObject(BooleanTypeKey)

  override def getStackSize: Int = 1

  object BooleanTypeKey

  override def description: String = "Defines the boolean type."
}
