package transformations.javac.types

import core.transformation.grammars.GrammarCatalogue
import core.transformation.{MetaObject, TransformationState}
import transformations.javac.base.model.QualifiedClassName

object ObjectTypeC extends TypeInstance {
  val stringType = objectType(new QualifiedClassName(Seq("java", "lang", "String")))

  override val key: AnyRef = ObjectTypeKey

  override def getSuperTypes(_type: MetaObject, state: TransformationState): Seq[MetaObject] = {
    Seq.empty //TODO extend
  }

  object ObjectTypeName

  object ObjectTypeKey

  def objectType(name: QualifiedClassName) = new MetaObject(ObjectTypeKey) {
    data.put(ObjectTypeName, Right(name))
  }

  def objectType(className: String) = new MetaObject(ObjectTypeKey) {
    data.put(ObjectTypeName, Left(className))
  }

  def getObjectTypeName(objectType: MetaObject): Either[String, QualifiedClassName] = objectType(ObjectTypeName).asInstanceOf[Either[String, QualifiedClassName]]

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val parseType = grammars.find(TypeC.TypeGrammar)
    val parseObjectType = identifier.someSeparated(".") ^^ { case ids: Seq[Any] =>
      val stringIds = ids.collect({ case v: String => v})
      if (ids.size > 1)
        objectType(new QualifiedClassName(stringIds))
      else
        objectType(stringIds(0))
    }
    parseType.inner = parseType.inner | parseObjectType
  }

  override def getByteCodeString(_type: MetaObject, state: TransformationState): String =
    s"L${getObjectTypeName(_type).right.get.parts.mkString("/")};"

  override def getStackSize: Int = 1
}
