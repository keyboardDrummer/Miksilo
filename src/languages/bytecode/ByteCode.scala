package languages.bytecode

import transformation.{TransformationState, ProgramTransformation, MetaObject}
import javaBytecode.ConstantPoolInfo

case class LineNumberRef(lineNumber: Int, startProgramCounter: Int)
trait StackMap
case class AppendFrame(offsetDelta: Int, localVerificationTypes: Seq[Any]) extends StackMap
case class SameFrame(offsetDelta: Int) extends StackMap
object ByteCode extends ProgramTransformation {
  object SourceFileAttribute
  object SourceFileFileNameIndex
  def sourceFile(nameIndex: Int, fileNameIndex: Int): MetaObject = new MetaObject(SourceFileAttribute) {
    data.put(SourceFileFileNameIndex, fileNameIndex)
    data.put(AttributeNameKey, nameIndex)
  }
  def getSourceFileFileNameIndex(sourceFile: MetaObject) = sourceFile(SourceFileFileNameIndex).asInstanceOf[Int]

  object AddressLoad
  def addressLoad(location: Int): MetaObject = instruction(AddressLoad, Seq(location))

  object IntegerStore

  object StackMapTableKey
  object StackMapTableMaps
  def stackMapTable(nameIndex: Int, stackMaps: Seq[StackMap]) = new MetaObject(StackMapTableKey) {
    data.put(AttributeNameKey, nameIndex)
    data.put(StackMapTableMaps, stackMaps)
  }
  def getStackMapTableEntries(stackMapTable: MetaObject) = stackMapTable(StackMapTableMaps).asInstanceOf[Seq[StackMap]]

  object StackMapTableId

  object LineNumberTableKey
  object LineNumberTableLines
  def lineNumberTable(nameIndex: Int, lines: Seq[LineNumberRef]) = new MetaObject(LineNumberTableKey) {
    data.put(AttributeNameKey, nameIndex)
    data.put(LineNumberTableLines, lines)
  }
  def getLineNumberTableEntries(lineNumberTable: MetaObject) = lineNumberTable(LineNumberTableLines).asInstanceOf[Seq[LineNumberRef]]

  object InvokeSpecial
  def invokeSpecial(location: Int): MetaObject = instruction(InvokeSpecial,Seq(location))

  object VoidReturn
  def voidReturn: MetaObject = instruction(VoidReturn)

  object LineNumberTableId
  object SourceFileId


  object NameAndTypeKey
  object NameAndTypeName
  object NameAndTypeType
  def nameAndType(nameIndex: Int, typeIndex: Int): MetaObject = new MetaObject(NameAndTypeKey) {
    data.put(NameAndTypeName, nameIndex)
    data.put(NameAndTypeType, typeIndex)
  }
  def getNameAndTypeName(nameAndType: MetaObject) = nameAndType(NameAndTypeName).asInstanceOf[Int]
  def getNameAndTypeType(nameAndType: MetaObject) = nameAndType(NameAndTypeType).asInstanceOf[Int]

  object ClassRefKey
  object ClassRefName
  def classRef(classRefNameIndex: Int): MetaObject = new MetaObject(ClassRefKey) {
    data.put(ClassRefName, classRefNameIndex)
  }
  def getClassRefName(classRef: MetaObject) = classRef(ClassRefName).asInstanceOf[Int]

  object MethodRefKey
  object MethodRefClassName
  object MethodRefMethodName
  def methodRef(classNameIndex: Int, methodNameAndTypeIndex: Int) = new MetaObject(MethodRefKey) {
    data.put(MethodRefClassName, classNameIndex)
    data.put(MethodRefMethodName, methodNameAndTypeIndex)
  }
  def getMethodRefClassRefIndex(methodRef: MetaObject) = methodRef(MethodRefClassName).asInstanceOf[Int]
  def getMethodRefMethodNameIndex(methodRef: MetaObject) = methodRef(MethodRefMethodName).asInstanceOf[Int]

  object IntegerReturn
  def integerReturn: MetaObject = instruction(IntegerReturn)

  object InvokeStaticKey
  def invokeStatic(constantIndex: Int): MetaObject = instruction(InvokeStaticKey, Seq(constantIndex))

  def instructionSize(instruction: MetaObject) = ???

  def goTo(target: Int): MetaObject = instruction(GoToKey, Seq(target))

  def ifIntegerCompareGreater(target: Int): MetaObject = instruction(IfIntegerCompareGreater, Seq(target))


  object MethodInfoKey
  object MethodNameIndex
  object MethodDescriptorIndex
  object MethodAnnotations
  object MethodAccessFlags
  object PublicAccess extends MethodAccessFlag
  object StaticAccess extends MethodAccessFlag
  object PrivateAccess extends MethodAccessFlag
  trait MethodAccessFlag
  def methodInfo(nameIndex: Int, descriptorIndex: Int, annotations: Seq[MetaObject], flags: Set[MethodAccessFlag] = Set()) =
    new MetaObject(MethodInfoKey)
  {
    data.put(MethodAnnotations, annotations)
    data.put(MethodNameIndex, nameIndex)
    data.put(MethodDescriptorIndex, descriptorIndex)
    data.put(MethodAccessFlags, flags)
  }
  def getMethodAccessFlags(method: MetaObject) = method(MethodAccessFlags).asInstanceOf[Set[MethodAccessFlag]]
  def getMethodAttributes(method: MetaObject) = method(MethodAnnotations).asInstanceOf[Seq[MetaObject]]
  def getMethodNameIndex(methodInfo: MetaObject) = methodInfo(MethodNameIndex).asInstanceOf[Int]
  def getMethodDescriptorIndex(methodInfo: MetaObject) = methodInfo(MethodDescriptorIndex).asInstanceOf[Int]


  object IntegerIncrementKey
  def integerIncrement(location: Int, amount: Int) = instruction(IntegerIncrementKey, Seq(location,amount))

  def integerStore(location: Int) = instruction(IntegerStore, Seq(location))

  object IntegerLoad
  def integerLoad(location: Integer) = instruction(IntegerLoad,Seq(location))
  def getInstructionStackSizeModification(metaObject: MetaObject) : Integer = -1

  object SubtractInteger
  def subtractInteger = instruction(SubtractInteger)

  object IntegerConstantKey
  object AddIntegersKey
  def addInteger = instruction(AddIntegersKey)

  def integerConstant(value: Int) = instruction(IntegerConstantKey, Seq(value))
  def getIntegerConstantValue(constant: MetaObject) = getInstructionArguments(constant)(0).asInstanceOf[Int]

  object InstructionArgumentsKey
  def instruction(_type: AnyRef, arguments: Seq[Any] = Seq()) = new MetaObject(_type) {
    data.put(InstructionArgumentsKey,arguments)
  }
  def getInstructionArguments(instruction: MetaObject) = instruction(InstructionArgumentsKey).asInstanceOf[Seq[Int]]
  def setInstructionArguments(instruction: MetaObject, arguments: Seq[Any]) {
    instruction(InstructionArgumentsKey) = arguments
  }

  object MethodDescriptor
  object MethodDescriptorParameters
  object MethodReturnType
  def ifNotEqual(target: Short) = new MetaObject("ifNotEquals")
  def methodDescriptor(returnDescriptor: Any, parameterDescriptors: Seq[Any]) = {
    new MetaObject(MethodDescriptor) {
      data.put(MethodDescriptorParameters, parameterDescriptors)
      data.put(MethodReturnType, returnDescriptor)
    }
  }
  def getMethodDescriptorReturnType(descriptor: MetaObject) = descriptor(MethodReturnType)

  def getMethodDescriptorParameters(desciptor: MetaObject) = desciptor(MethodDescriptorParameters).asInstanceOf[Seq[Any]]
  object AttributeKey
  object CodeAttributeId
  object CodeKey
  object AttributeNameKey
  object CodeMaxStackKey
  object CodeMaxLocalsKey
  object CodeInstructionsKey
  object CodeExceptionTableKey
  object CodeAttributesKey
  def codeAttribute(nameIndex: Integer, maxStack: Integer, maxLocals: Integer,
                    instructions: Seq[MetaObject],
                    exceptionTable: Seq[MetaObject],
                    attributes: Seq[MetaObject]) =
  {
    new MetaObject(CodeKey) {
      data.put(AttributeNameKey, nameIndex)
      data.put(CodeMaxStackKey, maxStack)
      data.put(CodeMaxLocalsKey, maxLocals)
      data.put(CodeInstructionsKey, instructions)
      data.put(CodeExceptionTableKey, exceptionTable)
      data.put(CodeAttributesKey, attributes)
    }
  }
  def getAttributeNameIndex(attribute: MetaObject) = attribute(AttributeNameKey).asInstanceOf[Int]
  def getCodeMaxStack(code: MetaObject) = code(CodeMaxStackKey).asInstanceOf[Int]
  def getCodeMaxLocals(code: MetaObject) = code(CodeMaxLocalsKey).asInstanceOf[Int]
  def getCodeExceptionTable(code: MetaObject) = code(CodeExceptionTableKey).asInstanceOf[Seq[MetaObject]]
  def getCodeAttributes(code: MetaObject) = code(CodeAttributesKey).asInstanceOf[Seq[MetaObject]]
  def getCodeInstructions(code: MetaObject) = code(CodeInstructionsKey).asInstanceOf[Seq[MetaObject]]

  object ClassFileKey
  object ClassMethodsKey
  object ClassNameIndexKey
  object ClassParentIndex
  object ClassConstantPool
  object ClassInterfaces
  object ClassFields
  object ClassAttributes
  def clazz(name: Int, parent: Int, constantPool: Seq[Any], methods: Seq[MetaObject], interfaces: Seq[Int] = Seq(),
             classFields: Seq[MetaObject] = Seq(), attributes: Seq[MetaObject] = Seq()) = new MetaObject(ClassFileKey) {
    data.put(ClassMethodsKey, methods)
    data.put(ClassNameIndexKey, name)
    data.put(ClassParentIndex, parent)
    data.put(ClassConstantPool, constantPool)
    data.put(ClassInterfaces, interfaces)
    data.put(ClassFields, classFields)
    data.put(ClassAttributes, attributes)
  }
  def getParentIndex(clazz: MetaObject) = clazz(ClassParentIndex).asInstanceOf[Int]
  def getConstantPool(clazz: MetaObject) = clazz(ClassConstantPool).asInstanceOf[Seq[Any]]
  def getClassNameIndex(clazz: MetaObject) = clazz(ClassNameIndexKey).asInstanceOf[Int]
  def getMethods(clazz: MetaObject) = clazz(ClassMethodsKey).asInstanceOf[Seq[MetaObject]]
  def getClassInterfaces(clazz: MetaObject) = clazz(ClassInterfaces).asInstanceOf[Seq[Int]]
  def getClassFields(clazz: MetaObject) = clazz(ClassFields).asInstanceOf[Seq[MetaObject]]
  def getClassAttributes(clazz: MetaObject) = clazz(ClassAttributes).asInstanceOf[Seq[MetaObject]]

  def transform(program: MetaObject, state: TransformationState): Unit = {}

  def dependencies: Set[ProgramTransformation] = Set.empty

  object GoToKey
  object IfIntegerCompareGreater
}
