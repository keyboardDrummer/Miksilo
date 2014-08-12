package transformations.types

import core.transformation.grammars.GrammarCatalogue
import core.transformation.{MetaObject, TransformationState}

object VoidTypeC extends TypeInstance {

  override val key: AnyRef = VoidTypeKey

  override def getSuperTypes(_type: MetaObject, state: TransformationState): Seq[MetaObject] = ???

  override def getByteCodeString(_type: MetaObject, state: TransformationState): String = "V"

  override def getStackSize: Int = 0

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val typeGrammar = grammars.find(TypeC.TypeGrammar)
    typeGrammar.inner = typeGrammar.inner | ("void" ^^ (_ => voidType))
  }

  def voidType = new MetaObject(VoidTypeKey)

  object VoidTypeKey

}
