package transformations.bytecode

import core.transformation.{Contract, MetaObject}

import scala.collection.mutable

case class LineNumberRef(lineNumber: Int, startProgramCounter: Int)

object ByteCode extends Contract {
  val pushNull = instruction(PushNull)

  def sameFrame(offset: Int) = new MetaObject(SameFrameKey) {
    data.put(OffsetDelta, offset)
  }

  def getFrameOffset(frame: MetaObject) = frame(OffsetDelta).asInstanceOf[Int]

  def sameFrameLocals1StackItem(offsetDelta: Int, _type: MetaObject) = new MetaObject(SameLocals1StackItem) {
    data.put(OffsetDelta, offsetDelta)
    data.put(SameLocals1StackItemType, _type)
  }

  def getSameLocals1StackItemType(sameLocals1StackItem: MetaObject) = sameLocals1StackItem(SameLocals1StackItemType).asInstanceOf[MetaObject]

  def appendFrame(offset: Int, newLocalTypes: Seq[MetaObject]) = new MetaObject(AppendFrame) {
    data.put(OffsetDelta, offset)
    data.put(AppendFrameTypes, newLocalTypes)
  }

  def getAppendFrameTypes(appendFrame: MetaObject) = appendFrame(AppendFrameTypes).asInstanceOf[Seq[MetaObject]]

  def ifZero(target: Int) = instruction(IfZeroKey, Seq(target))

  def invokeVirtual(methodRefIndex: Int) = instruction(InvokeVirtual, Seq(methodRefIndex))

  def getStatic(fieldRefIndex: Int): MetaObject = instruction(GetStatic, Seq(fieldRefIndex))

  def sourceFile(nameIndex: Int, fileNameIndex: Int): MetaObject = new MetaObject(SourceFileAttribute) {
    data.put(SourceFileFileNameIndex, fileNameIndex)
    data.put(AttributeNameKey, nameIndex)
  }

  def getSourceFileFileNameIndex(sourceFile: MetaObject) = sourceFile(SourceFileFileNameIndex).asInstanceOf[Int]

  def addressStore(location: Int): MetaObject = instruction(AddressStore, Seq(location))

  def addressLoad(location: Int): MetaObject = instruction(AddressLoad, Seq(location))

  def stackMapTable(nameIndex: Int, stackMaps: Seq[MetaObject]) = new MetaObject(StackMapTableKey) {
    data.put(AttributeNameKey, nameIndex)
    data.put(StackMapTableMaps, stackMaps)
  }

  def getStackMapTableEntries(stackMapTable: MetaObject) = stackMapTable(StackMapTableMaps).asInstanceOf[Seq[MetaObject]]

  def lineNumberTable(nameIndex: Int, lines: Seq[LineNumberRef]) = new MetaObject(LineNumberTableKey) {
    data.put(AttributeNameKey, nameIndex)
    data.put(LineNumberTableLines, lines)
  }

  def getLineNumberTableEntries(lineNumberTable: MetaObject) = lineNumberTable(LineNumberTableLines).asInstanceOf[Seq[LineNumberRef]]

  def invokeSpecial(location: Int): MetaObject = instruction(InvokeSpecial, Seq(location))

  def voidReturn: MetaObject = instruction(VoidReturn)

  def nameAndType(nameIndex: Int, typeIndex: Int): MetaObject = new MetaObject(NameAndTypeKey) {
    data.put(NameAndTypeName, nameIndex)
    data.put(NameAndTypeType, typeIndex)
  }

  def getNameAndTypeName(nameAndType: MetaObject) = nameAndType(NameAndTypeName).asInstanceOf[Int]

  def getNameAndTypeType(nameAndType: MetaObject) = nameAndType(NameAndTypeType).asInstanceOf[Int]

  def classRef(classRefNameIndex: Int): MetaObject = new MetaObject(ClassRefKey) {
    data.put(ClassRefName, classRefNameIndex)
  }

  def getClassRefName(classRef: MetaObject) = classRef(ClassRefName).asInstanceOf[Int]

  def methodRef(classNameIndex: Int, methodNameAndTypeIndex: Int) = new MetaObject(MethodRefKey) {
    data.put(MethodRefClassName, classNameIndex)
    data.put(MethodRefMethodName, methodNameAndTypeIndex)
  }

  def getMethodRefClassRefIndex(methodRef: MetaObject) = methodRef(MethodRefClassName).asInstanceOf[Int]

  def getMethodRefMethodNameIndex(methodRef: MetaObject) = methodRef(MethodRefMethodName).asInstanceOf[Int]

  def integerReturn: MetaObject = instruction(IntegerReturn)

  def invokeStatic(constantIndex: Int): MetaObject = instruction(InvokeStaticKey, Seq(constantIndex))

  def instruction(_type: AnyRef, arguments: Seq[Any] = Seq()) = new MetaObject(_type) {
    data.put(InstructionArgumentsKey, arguments)
  }

  def instructionSize(instruction: MetaObject) = ???

  def goTo(target: Int): MetaObject = instruction(GoToKey, Seq(target))

  def ifIntegerCompareGreater(target: Int): MetaObject = instruction(IfIntegerCompareGreater, Seq(target))

  def methodInfo(nameIndex: Int, descriptorIndex: Int, annotations: Seq[MetaObject], flags: Set[MethodAccessFlag] = Set()) =
    new MetaObject(MethodInfoKey) {
      data.put(MethodAnnotations, annotations)
      data.put(MethodNameIndex, nameIndex)
      data.put(MethodDescriptorIndex, descriptorIndex)
      data.put(MethodAccessFlags, flags)
    }

  def getMethodAccessFlags(method: MetaObject) = method(MethodAccessFlags).asInstanceOf[Set[MethodAccessFlag]]

  def getMethodAttributes(method: MetaObject) = method(MethodAnnotations).asInstanceOf[Seq[MetaObject]]

  def getMethodNameIndex(methodInfo: MetaObject) = methodInfo(MethodNameIndex).asInstanceOf[Int]

  def getMethodDescriptorIndex(methodInfo: MetaObject) = methodInfo(MethodDescriptorIndex).asInstanceOf[Int]

  def integerIncrement(location: Int, amount: Int) = instruction(IntegerIncrementKey, Seq(location, amount))

  def integerStore(location: Int) = instruction(IntegerStore, Seq(location))

  def integerLoad(location: Integer) = instruction(IntegerLoad, Seq(location))

  def subtractInteger = instruction(SubtractInteger)

  def addInteger = instruction(AddIntegersKey)

  def integerConstant(value: Int) = instruction(IntegerConstantKey, Seq(value))

  def getIntegerConstantValue(constant: MetaObject) = getInstructionArguments(constant)(0).asInstanceOf[Int]

  def getInstructionArguments(instruction: MetaObject) = instruction(InstructionArgumentsKey).asInstanceOf[Seq[Int]]

  def setInstructionArguments(instruction: MetaObject, arguments: Seq[Any]) {
    instruction(InstructionArgumentsKey) = arguments
  }

  def constantPoolGet(constantPool: Seq[Any], index: Int) = constantPool(index - 1)

  def methodDescriptor(returnDescriptor: MetaObject, parameterDescriptors: Seq[MetaObject]) = {
    new MetaObject(MethodDescriptor) {
      data.put(MethodDescriptorParameters, parameterDescriptors)
      data.put(MethodReturnType, returnDescriptor)
    }
  }

  def getMethodDescriptorReturnType(descriptor: MetaObject) = descriptor(MethodReturnType).asInstanceOf[MetaObject]

  def getMethodDescriptorParameters(descriptor: MetaObject) = descriptor(MethodDescriptorParameters).asInstanceOf[Seq[MetaObject]]

  def codeAttribute(nameIndex: Integer, maxStack: Integer, maxLocals: Integer,
                    instructions: Seq[MetaObject],
                    exceptionTable: Seq[MetaObject],
                    attributes: Seq[MetaObject]) = {
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

  def clazz(name: Int, parent: Int, constantPool: mutable.Buffer[Any], methods: Seq[MetaObject], interfaces: Seq[Int] = Seq(),
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

  def getConstantPool(clazz: MetaObject) = clazz(ClassConstantPool).asInstanceOf[mutable.Buffer[Any]]

  def getClassNameIndex(clazz: MetaObject) = clazz(ClassNameIndexKey).asInstanceOf[Int]

  def getMethods(clazz: MetaObject) = clazz(ClassMethodsKey).asInstanceOf[Seq[MetaObject]]

  def getClassInterfaces(clazz: MetaObject) = clazz(ClassInterfaces).asInstanceOf[Seq[Int]]

  def getClassFields(clazz: MetaObject) = clazz(ClassFields).asInstanceOf[Seq[MetaObject]]

  def getClassAttributes(clazz: MetaObject) = clazz(ClassAttributes).asInstanceOf[Seq[MetaObject]]

  def fieldRef(classIndex: Int, nameAndTypeIndex: Int) = new MetaObject(FieldRef) {
    data.put(FieldRefClassIndex, classIndex)
    data.put(FieldRefNameAndTypeIndex, nameAndTypeIndex)
  }

  def getFieldRefClassIndex(fieldRef: MetaObject) = fieldRef(FieldRefClassIndex).asInstanceOf[Int]

  def getFieldRefNameAndTypeIndex(fieldRef: MetaObject) = fieldRef(FieldRefNameAndTypeIndex).asInstanceOf[Int]

  override def dependencies: Set[Contract] = Set.empty

  trait MethodAccessFlag


  object PushNull

  object SameFrameKey

  object OffsetDelta

  object SameLocals1StackItem

  object SameLocals1StackItemType

  object AppendFrame

  object AppendFrameTypes

  object FullFrame

  object FullFrameLocals

  object FullFrameStack

  object ChopFrame

  object ChopFrameCount

  object IfZeroKey

  object InvokeVirtual


  object GetStatic

  object SourceFileAttribute

  object SourceFileFileNameIndex

  object AddressStore

  object AddressLoad

  object IntegerStore

  object StackMapTableKey

  object StackMapTableMaps

  object StackMapTableId

  object LineNumberTableKey

  object LineNumberTableLines

  object InvokeSpecial

  object VoidReturn

  object LineNumberTableId

  object SourceFileId

  object NameAndTypeKey

  object NameAndTypeName

  object NameAndTypeType

  object ClassRefKey

  object ClassRefName

  object MethodRefKey

  object MethodRefClassName

  object MethodRefMethodName

  object IntegerReturn

  object InvokeStaticKey

  object MethodInfoKey

  object MethodNameIndex

  object MethodDescriptorIndex

  object MethodAnnotations

  object MethodAccessFlags

  object PublicAccess extends MethodAccessFlag

  object StaticAccess extends MethodAccessFlag

  object PrivateAccess extends MethodAccessFlag

  object IntegerIncrementKey

  object IntegerLoad

  object SubtractInteger

  object IntegerConstantKey

  object AddIntegersKey

  object InstructionArgumentsKey

  object MethodDescriptor

  object MethodDescriptorParameters

  object MethodReturnType

  object AttributeKey

  object CodeAttributeId

  object CodeKey

  object AttributeNameKey

  object CodeMaxStackKey

  object CodeMaxLocalsKey

  object CodeInstructionsKey

  object CodeExceptionTableKey

  object CodeAttributesKey

  object ClassFileKey

  object ClassMethodsKey

  object ClassNameIndexKey

  object ClassParentIndex

  object ClassConstantPool

  object ClassInterfaces

  object ClassFields

  object ClassAttributes

  object GoToKey

  object IfIntegerCompareGreater

  object FieldRef

  object FieldRefClassIndex

  object FieldRefNameAndTypeIndex

}
