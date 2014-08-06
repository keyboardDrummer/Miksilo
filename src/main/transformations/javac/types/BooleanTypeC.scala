package transformations.javac.types

import core.transformation.grammars.GrammarCatalogue
import core.transformation.{MetaObject, TransformationState}

object BooleanTypeC extends TypeInstance {
  override val key: AnyRef = BooleanTypeKey

  def booleanType = new MetaObject(BooleanTypeKey)

  object BooleanTypeKey

  override def getSuperTypes(_type: MetaObject, state: TransformationState): Seq[MetaObject] = Seq.empty

  override def getByteCodeString(_type: MetaObject, state: TransformationState): String = "Z"

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val typeGrammar = grammars.find(TypeC.TypeGrammar)
    typeGrammar.inner = ("boolean" ^^ (_ => booleanType)) | typeGrammar.inner
  }

  override def getStackSize: Int = 1
}
