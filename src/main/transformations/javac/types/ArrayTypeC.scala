package transformations.javac.types

import core.grammar.seqr
import core.transformation.grammars.GrammarCatalogue
import core.transformation.{MetaObject, TransformationState}

object ArrayTypeC extends TypeInstance {
  override val key: AnyRef = ArrayTypeKey

  object ArrayTypeKey

  object ArrayElementType

  override def getSuperTypes(_type: MetaObject, state: TransformationState): Seq[MetaObject] = Seq.empty

  override def getByteCodeString(_type: MetaObject, state: TransformationState): String =
    s"[${TypeC.getByteCodeString(state)(getArrayElementType(_type))}"

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val parseType = grammars.find(TypeC.TypeGrammar)
    val parseArrayType = parseType ~ "[]" ^^ { case _type seqr _ => arrayType(_type.asInstanceOf[MetaObject])}
    parseType.inner = parseType.inner | parseArrayType
  }

  def arrayType(elementType: MetaObject) = {
    new MetaObject(ArrayTypeKey) {
      data.put(ArrayElementType, elementType)
    }
  }


  def getArrayElementType(arrayType: MetaObject) = arrayType(ArrayElementType).asInstanceOf[MetaObject]

  override def getStackSize: Int = 1
}
