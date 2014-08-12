package transformations.types

import core.transformation.grammars.GrammarCatalogue
import core.transformation.{MetaObject, TransformationState}
import transformations.types.TypeC.TypeGrammar

object IntTypeC extends TypeInstance {

  override val key: AnyRef = IntTypeKey

  override def getSuperTypes(_type: MetaObject, state: TransformationState): Seq[MetaObject] = Seq.empty //TODO extend. long ?

  override def getByteCodeString(_type: MetaObject, state: TransformationState): String = "I"

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val parseType = grammars.create(TypeGrammar)
    val parseIntType = "int" ^^ (_ => IntTypeC.intType)
    parseType.inner = parseType.inner | parseIntType

  }

  def intType = new MetaObject(IntTypeKey)

  override def getStackSize: Int = 1

  object IntTypeKey

}
