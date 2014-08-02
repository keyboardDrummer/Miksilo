package transformations.javac.base.model

import core.transformation.MetaObject

object JavaTypes {
  val stringType = objectType(new QualifiedClassName(Seq("java", "lang", "String")))

  def voidType = new MetaObject(VoidTypeKey)

  def booleanType = new MetaObject(BooleanTypeKey)

  def javaTypeToByteCodeType(_type: MetaObject) = _type.clazz match {
    case BooleanTypeKey => new MetaObject(IntTypeKey)
    case _ => _type
  }

  def objectType(name: QualifiedClassName) = new MetaObject(ObjectTypeKey) {
    data.put(ObjectTypeName, Right(name))
  }

  def objectType(className: String) = new MetaObject(ObjectTypeKey) {
    data.put(ObjectTypeName, Left(className))
  }

  def getObjectTypeName(objectType: MetaObject): Either[String, QualifiedClassName] = objectType(ObjectTypeName).asInstanceOf[Either[String, QualifiedClassName]]

  def intType = new MetaObject(IntTypeKey)

  def arrayType(elementType: MetaObject) = {
    new MetaObject(ArrayTypeKey) {
      data.put(ArrayElementType, elementType)
    }
  }

  def getArrayElementType(arrayType: MetaObject) = arrayType(ArrayElementType).asInstanceOf[MetaObject]

  object BooleanTypeKey

  object ObjectTypeName

  object ObjectTypeKey

  object VoidTypeKey

  object IntTypeKey

  object LongTypeKey

  object DoubleTypeKey

  object ArrayTypeKey

  object ArrayElementType

}
