package transformations.types

import core.transformation.grammars.GrammarCatalogue
import core.transformation.{MetaObject, CompilationState}

object BooleanTypeC extends TypeInstance {
  override val key: AnyRef = BooleanTypeKey

  override def getSuperTypes(_type: MetaObject, state: CompilationState): Seq[MetaObject] = Seq.empty

  override def getByteCodeString(_type: MetaObject, state: CompilationState): String = "Z"

  override def getJavaGrammar(grammars: GrammarCatalogue) = {
    "boolean" ~> produce(booleanType)
  }

  def booleanType = new MetaObject(BooleanTypeKey)

  override def getStackSize: Int = 1

  object BooleanTypeKey

  override def description: String = "Defines the boolean type."
}
