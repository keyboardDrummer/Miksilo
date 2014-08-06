package transformations.javac.types

import core.transformation.grammars.GrammarCatalogue
import core.transformation.{MetaObject, TransformationState}
import transformations.javac.types.TypeC.TypeGrammar

object IntTypeC extends TypeInstance {

  object IntTypeKey

  override val key: AnyRef = IntTypeKey

  override def getSuperTypes(_type: MetaObject, state: TransformationState): Seq[MetaObject] = Seq.empty //TODO extend. long ?

  def intType = new MetaObject(IntTypeKey)

  override def getByteCodeString(_type: MetaObject, state: TransformationState): String = "I"

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val parseType = grammars.create(TypeGrammar)
    val parseIntType = "int" ^^ (_ => IntTypeC.intType)
    parseType.inner = parseType.inner | parseIntType

  }

  override def getStackSize: Int = 1
}
