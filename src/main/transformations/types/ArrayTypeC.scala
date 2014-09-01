package transformations.types


import core.transformation.grammars.GrammarCatalogue
import core.transformation.{MetaObject, TransformationState}

object ArrayTypeC extends TypeInstance {
  override val key: AnyRef = ArrayTypeKey

  override def getSuperTypes(_type: MetaObject, state: TransformationState): Seq[MetaObject] = Seq.empty

  override def getByteCodeString(_type: MetaObject, state: TransformationState): String =
    s"[${TypeC.getByteCodeString(state)(getArrayElementType(_type))}"

  def getArrayElementType(arrayType: MetaObject) = arrayType(ArrayElementType).asInstanceOf[MetaObject]

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val parseType = grammars.find(TypeC.TypeGrammar)
    val parseArrayType = parseType <~ "[]" ^^ parseMap(ArrayTypeKey, ArrayElementType)
    parseType.orToInner(parseArrayType)
  }

  def arrayType(elementType: MetaObject) = {
    new MetaObject(ArrayTypeKey) {
      data.put(ArrayElementType, elementType)
    }
  }

  override def getStackSize: Int = 1


  object ArrayTypeKey

  object ArrayElementType

}
