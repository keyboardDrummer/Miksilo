package typed.languages.javaBytecode

class Identifier(value: String)
class ClassFile(constantPool: Seq[ConstantPoolInfo],
                accessFlags: AccessFlags, name: String, sup: Option[Identifier], interfaces: Set[Identifier],
                fields: Seq[FieldInfo], methods: Seq[MethodInfo], attributes: Seq[AttributeInfo]) {

}

class AccessFlags
class FieldInfo(accessFlags: FieldAccessFlags, nameIndex: Integer, descriptorIndex: Integer,
                 attributes: Seq[AttributeInfo])
abstract class AttributeInfo(nameIndex: Integer)
class ConstantAttribute(index: Integer)
{

}
class CodeAttribute(maxStack: Integer, maxLocals: Integer, code: Seq[Instruction], exceptions: Seq[ExceptionEntry])
class ExceptionEntry(startPc: Integer, endPc: Integer, handlerPc: Integer, catchType: Integer)

class MethodInfo(accessFlags: MethodAccessFlags, nameIndex: Integer, descriptorIndex: Integer,
attributes: Seq[AttributeInfo])

class MethodAccessFlags(_public: Boolean, _private: Boolean, _protected: Boolean, _static: Boolean, _final: Boolean,
                         _synchronized: Boolean, _bridge: Boolean, _varargs: Boolean, _native: Boolean, _abstract: Boolean,
                         _strict: Boolean, _synthetic: Boolean)
class FieldAccessFlags(_public: Boolean, _protected: Boolean, _private: Boolean,
                        _static: Boolean, _final: Boolean, _volatile: Boolean, _transient: Boolean, _enum: Boolean)