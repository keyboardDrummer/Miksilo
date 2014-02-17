package languages.bytecode

import transformation.{TransformationState, ProgramTransformation, MetaObject}
import javaBytecode.ConstantPoolInfo

object ByteCode extends ProgramTransformation {
  def goTo(target: Int): MetaObject = instruction("goTo", Seq(target))

  def ifIntegerCompareGreater(target: Int): MetaObject = instruction("ifIntegerCompareGreater", Seq(target))


  def methodInfo(nameIndex: Int, descriptorIndex: Int, annotations: Seq[MetaObject]) = new MetaObject(MethodInfoKey) {
    
  }
  object MethodInfoKey
  object MethodNameIndex
  object MethodDescriptorIndex
  object MethodAnnotations


  def integerIncrement(location: Int, amount: Int) = instruction("integerIncrement", Seq(location,amount))

  def addressStore(location: Int) = instruction("addressStore", Seq(location))

  def addressLoad(location: Integer) = instruction("loadAddress",Seq(location))
  def getInstructionStackSizeModification(metaObject: MetaObject) : Integer = ???

  def subtractInteger = instruction("subtractIntegers")

  def addInteger = instruction("addIntegers")

  def integerConstant(value: Integer) = instruction("integerConstant", Seq(value))

  def instruction(_type: String, arguments: Seq[Any] = Seq()) = new MetaObject(_type) {
    data.put("arguments",arguments)
  }

  def ifNotEqual(target: Short) = new MetaObject("ifNotEquals")
  def methodDescriptor(returnDescriptor: MetaObject, parameterDescriptors: Seq[MetaObject]) = {
    new MetaObject("methodDescriptor") {
      data.put("parameters", parameterDescriptors)
      data.put("returnType", returnDescriptor)
    }
  }

  def codeAttribute(nameIndex: Integer, length: Integer, maxStack: Integer, maxLocals: Integer,
                    code: Seq[MetaObject],
                    exceptionTable: Seq[MetaObject],
                    attributes: Seq[MetaObject]) =
  {
    ???
  }

  val nameIndex: String = "nameIndex"

  object ClassFileKey
  object ClassMethodsKey
  object ClassNameKey
  object ClassConstantPool
  def clazz(name: String, constantPool: Seq[MetaObject], methods: Seq[MetaObject]) = new MetaObject(ClassFileKey) {
    data.put(ClassMethodsKey, methods)
    data.put(ClassNameKey, name)
    data.put(ClassConstantPool, constantPool)
  }


  def transform(program: MetaObject, state: TransformationState): Unit = {}

  def dependencies: Set[ProgramTransformation] = Set.empty
}
