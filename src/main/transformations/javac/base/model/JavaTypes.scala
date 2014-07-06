package transformations.javac.base.model

import core.transformation.MetaObject

object JavaTypes {
   object BooleanType

   def javaTypeToByteCodeType(_type: Any) = _type match {
     case BooleanType => IntType
     case _ => _type
   }

   def objectType(name: QualifiedClassName): Any = new MetaObject(ObjectType) {
     data.put(ObjectTypeName, Right(name))
   }
   def objectType(className: String): Any = new MetaObject(ObjectType) {
     data.put(ObjectTypeName, Left(className))
   }
   def getObjectTypeName(objectType: MetaObject): Either[String,QualifiedClassName] = objectType(ObjectTypeName).asInstanceOf[Either[String,QualifiedClassName]]

   val stringType = objectType("String")

   object ObjectTypeName
   object ObjectType
   object VoidType
   object IntType
   object LongType
   object DoubleType
   object ArrayType
   object ArrayElementType
   def arrayType(elementType: Any) = {
     new MetaObject(ArrayType) { data.put(ArrayElementType, elementType) }
   }
   def getArrayElementType(arrayType: MetaObject): Any = arrayType(ArrayElementType)

 }
