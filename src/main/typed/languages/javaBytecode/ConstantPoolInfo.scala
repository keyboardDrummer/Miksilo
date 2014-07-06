package typed.languages.javaBytecode

class ConstantPoolInfo {

}

class ConstantClass(classIndex: Integer)
class ConstantFieldRef(classIndex: Integer, nameAndTypeIndex: Integer)
class ConstantMethodRef(classIndex: Integer, nameAndTypeIndex: Integer)
class ConstantInterfaceMethodRef(classIndex: Integer, nameAndTypeIndex: Integer)
class ConstantString(index: Integer)
class ConstantUtf8(string: String)
class ConstantFloat(float: Float)
class ConstantInteger(integer: Integer)
class ConstantLong(long: Long)
class ConstantDouble(double: Double)
class ConstantNameAndType(nameIndex: Integer, descriptorIndex: Integer)
class ConstantMethodHandle(kind: MethodHandleKind, referenceIndex: Integer)
class MethodHandleKind
class GetFieldKind extends MethodHandleKind
class GetStaticKind extends MethodHandleKind
class PutFieldKind extends MethodHandleKind
class PutStaticKind extends MethodHandleKind
class InvokeVirtualKind extends MethodHandleKind
class InvokeStaticKind extends MethodHandleKind
class InvokeSpecialKind extends MethodHandleKind
class NewInvokeSpecialKind extends MethodHandleKind
class InvokeInterfaceKind extends MethodHandleKind
class ConstantMethodType(descriptorIndex: Integer)
class ConstantInvokeDynamic(bootstrapMethodIndex: Integer, nameAndTypeIndex: Integer)