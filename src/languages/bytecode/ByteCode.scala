package languages.bytecode

import transformation.{TransformationState, ProgramTransformation, MetaObject}
import javaBytecode.ConstantPoolInfo

case class LineNumberRef(lineNumber: Int, startProgramCounter: Int)
object ByteCode extends ProgramTransformation {
  object LineNumberKey
  object LineNumberTableLines
  def lineNumberTable(nameIndex: Int, length: Int, lines: Seq[LineNumberRef]) = new MetaObject(LineNumberKey) {
    data.put(AttributeNameKey, nameIndex)
    data.put(AttributeLengthKey, length)
    data.put(LineNumberTableLines, lines)
  }

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

  object ClassRefKey
  object ClassRefName
  def classRef(classRefNameIndex: Int): MetaObject = new MetaObject(ClassRefKey) {
    data.put(ClassRefName, classRefNameIndex)
  }


  object MethodRefKey
  object MethodRefClassName
  object MethodRefMethodName
  def methodRef(classNameIndex: Int, methodNameAndTypeIndex: Int) = new MetaObject(MethodRefKey) {
    data.put(MethodRefClassName, classNameIndex)
    data.put(MethodRefMethodName, methodNameAndTypeIndex)
  }

  object IntegerReturn
  def integerReturn: MetaObject = instruction(IntegerReturn)

  object InvokeStaticKey
  def invokeStatic(constantIndex: Int): MetaObject = instruction(InvokeStaticKey, Seq(constantIndex))

  def instructionSize(instruction: MetaObject) = ???

  def goTo(target: Int): MetaObject = instruction(GoToKey, Seq(target))

  def ifIntegerCompareGreater(target: Int): MetaObject = instruction(IfIntegerCompareGreater, Seq(target))


  def getMethodAnnotations(method: MetaObject) = method(MethodAnnotations).asInstanceOf[Seq[MetaObject]]
  def methodInfo(nameIndex: Int, descriptorIndex: Int, annotations: Seq[MetaObject]) = new MetaObject(MethodInfoKey)
  {
    data.put(MethodAnnotations, annotations)
    data.put(MethodNameIndex, nameIndex)
    data.put(MethodDescriptorIndex, descriptorIndex)
  }
  object MethodInfoKey
  object MethodNameIndex
  object MethodDescriptorIndex
  object MethodAnnotations


  object IntegerIncrementKey
  def integerIncrement(location: Int, amount: Int) = instruction(IntegerIncrementKey, Seq(location,amount))

  def addressStore(location: Int) = instruction("addressStore", Seq(location))

  def addressLoad(location: Integer) = instruction("loadAddress",Seq(location))
  def getInstructionStackSizeModification(metaObject: MetaObject) : Integer = -1

  def subtractInteger = instruction("subtractIntegers")

  object IntegerConstantKey
  object AddIntegersKey
  def addInteger = instruction(AddIntegersKey)

  def integerConstant(value: Integer) = instruction(IntegerConstantKey, Seq(value))

  object InstructionArgumentsKey
  def instruction(_type: AnyRef, arguments: Seq[Any] = Seq()) = new MetaObject(_type) {
    data.put(InstructionArgumentsKey,arguments)
  }
  def getInstructionArguments(instruction: MetaObject) = instruction(InstructionArgumentsKey).asInstanceOf[Seq[Any]]
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

  object AttributeKey
  object CodeAttributeId
  object CodeKey
  object AttributeNameKey
  object AttributeLengthKey
  object CodeMaxStackKey
  object CodeMaxLocalsKey
  object CodeInstructionsKey
  object CodeExceptionTableKey
  object CodeAttributesKey
  def codeAttribute(nameIndex: Integer, length: Integer, maxStack: Integer, maxLocals: Integer,
                    instructions: Seq[MetaObject],
                    exceptionTable: Seq[MetaObject],
                    attributes: Seq[MetaObject]) =
  {
    new MetaObject(CodeKey) {
      data.put(AttributeNameKey, nameIndex)
      data.put(AttributeLengthKey, length)
      data.put(CodeMaxStackKey, maxStack)
      data.put(CodeMaxLocalsKey, maxLocals)
      data.put(CodeInstructionsKey, instructions)
      data.put(CodeExceptionTableKey, exceptionTable)
      data.put(CodeAttributesKey, attributes)
    }
  }

  def getCodeInstructions(code: MetaObject) = code(CodeInstructionsKey).asInstanceOf[Seq[MetaObject]]

  object ClassFileKey
  object ClassMethodsKey
  object ClassNameKey
  object ClassConstantPool
  def clazz(name: String, constantPool: Seq[Any], methods: Seq[MetaObject]) = new MetaObject(ClassFileKey) {
    data.put(ClassMethodsKey, methods)
    data.put(ClassNameKey, name)
    data.put(ClassConstantPool, constantPool)
  }
  def getConstantPool(clazz: MetaObject) = clazz(ClassConstantPool).asInstanceOf[Seq[Any]]

  def getMethods(clazz: MetaObject) = clazz(ClassMethodsKey).asInstanceOf[Seq[MetaObject]]

  def transform(program: MetaObject, state: TransformationState): Unit = {}

  def dependencies: Set[ProgramTransformation] = Set.empty

  object GoToKey
  object IfIntegerCompareGreater
}
