package javaBytecode

class ReturnType
class VoidType extends ReturnType
class FieldType extends ReturnType

class ClassType(name: Identifier) extends FieldType {

}

class ByteType extends FieldType
class CharType extends FieldType
class DoubleType extends FieldType
class FloatType extends FieldType
class IntType extends FieldType
class LongType extends FieldType
class ShortType extends FieldType
class BooleanType extends FieldType
class ArrayType(elementType: FieldType)