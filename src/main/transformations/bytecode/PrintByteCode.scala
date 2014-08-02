package transformations.bytecode

import java.math.BigInteger

import akka.util.Convert
import core.transformation.MetaObject
import transformations.bytecode.ByteCode._
import transformations.javac.base.model.JavaTypes.{ArrayTypeKey, ObjectTypeKey}
import transformations.javac.base.model.{JavaTypes, QualifiedClassName}

object PrintByteCode {
  val accessFlags: Map[String, Int] = Map("super" -> 0x0020)
  var debugCounter: Int = 0

  def print(byteCode: MetaObject): String = {
    formatHexLikeClassFile(valueOf(getBytes(byteCode))).toLowerCase
  }

  def formatHexLikeClassFile(hex: String): String = {
    hex.grouped(4).grouped(8).map(g => g.mkString(" ")).mkString("\n")
  }

  def valueOf(buf: Iterable[Byte]): String = buf.map("%02X" format _).mkString

  def getBytes(byteCode: MetaObject): Seq[Byte] = {
    val clazz = byteCode
    var result = List[Byte]()

    result ++= intToBytes(0xCAFEBABE)
    result ++= intToBytes(0x00000033)
    val constantPool = ByteCode.getConstantPool(clazz)
    val constantPoolSizePlusOne = shortToBytes(constantPool.length + 1)
    result ++= constantPoolSizePlusOne
    for (constantPoolEntry <- constantPool) {
      result ++= getConstantEntryByteCode(constantPoolEntry)
    }
    result ++= getAccessFlagsByteCode(clazz)
    result ++= shortToBytes(ByteCode.getClassNameIndex(clazz))
    result ++= shortToBytes(ByteCode.getParentIndex(clazz))
    result ++= getInterfacesByteCode(clazz)
    result ++= getFieldsByteCode(clazz)
    result ++= getMethodsByteCode(clazz)
    result ++= getAttributesByteCode(ByteCode.getClassAttributes(clazz))
    result
  }

  def getMethodsByteCode(clazz: MetaObject): Seq[Byte] = {
    val methods = ByteCode.getMethods(clazz)
    shortToBytes(methods.length) ++ methods.flatMap(method => getMethodByteCode(method))
  }

  def getMethodByteCode(methodInfo: MetaObject) = {
    val accessCodes = Map(
      ByteCode.PublicAccess -> "0001",
      ByteCode.StaticAccess -> "0008",
      ByteCode.PrivateAccess -> "0002").mapValues(s => hexToInt(s))
    shortToBytes(ByteCode.getMethodAccessFlags(methodInfo).map(flag => accessCodes(flag)).sum) ++
      shortToBytes(ByteCode.getMethodNameIndex(methodInfo)) ++
      shortToBytes(ByteCode.getMethodDescriptorIndex(methodInfo)) ++
      getAttributesByteCode(ByteCode.getMethodAttributes(methodInfo))
  }

  def getAttributesByteCode(attributes: Seq[MetaObject]) = {
    shortToBytes(attributes.length) ++ attributes.flatMap(attribute => getAttributeByteCode(attribute))
  }

  def getExceptionByteCode(exception: MetaObject): Seq[Byte] = ???

  def hexToInt(hex: String): Int = new BigInteger(hex, 16).intValue()

  def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    val arguments = ByteCode.getInstructionArguments(instruction)
    val result = instruction.clazz match {
      case ByteCode.GetStatic => hexToBytes("b2") ++ shortToBytes(arguments(0))
      case ByteCode.AddIntegersKey => hexToBytes("60")
      case ByteCode.SubtractInteger => hexToBytes("64")
      case ByteCode.IntegerConstantKey =>
        byteToBytes(3 + ByteCode.getIntegerConstantValue(instruction))
      case ByteCode.AddressLoad =>
        val location = arguments(0)
        if (location > 3)
          hexToBytes("19") ++ byteToBytes(location)
        else
          byteToBytes(hexToInt("2a") + location)
      case ByteCode.IntegerLoad =>
        val location = arguments(0)
        if (location > 3)
          hexToBytes("15") ++ byteToBytes(location)
        else
          byteToBytes(hexToInt("1a") + location)
      case ByteCode.GoToKey => hexToBytes("a7") ++ shortToBytes(arguments(0))
      case ByteCode.InvokeVirtual => hexToBytes("b6") ++ shortToBytes(arguments(0))
      case ByteCode.InvokeSpecial => hexToBytes("b7") ++ shortToBytes(arguments(0))
      case ByteCode.InvokeStaticKey => hexToBytes("b8") ++ shortToBytes(arguments(0))
      case ByteCode.VoidReturn => hexToBytes("b1")
      case ByteCode.IntegerReturn => hexToBytes("ac")
      case ByteCode.IntegerStore =>
        val location = arguments(0)
        if (location > 3)
          hexToBytes("36") ++ byteToBytes(location)
        else
          byteToBytes(hexToInt("3b") + location)
      case ByteCode.IfIntegerCompareGreater => hexToBytes("a2") ++ shortToBytes(arguments(0))
      case ByteCode.IfZeroKey => hexToBytes("99") ++ shortToBytes(arguments(0))
      case ByteCode.IntegerIncrementKey => hexToBytes("84") ++
        byteToBytes(arguments(0)) ++
        byteToBytes(arguments(1))
      case PushNull => hexToBytes("01")
      case AddressStore =>
        val location = arguments(0)
        if (location > 3)
          hexToBytes("3a") ++ byteToBytes(location)
        else
          byteToBytes(hexToInt("4b") + location)
    }
    result
  }

  def getAttributeByteCode(attribute: MetaObject): Seq[Byte] = {
    shortToBytes(ByteCode.getAttributeNameIndex(attribute)) ++
      prefixWithIntLength(() => attribute.clazz match {
        case ByteCode.CodeKey =>
          getCodeAttributeBytes(attribute)
        case ByteCode.LineNumberTableKey =>
          getLineNumberTableBytes(attribute)
        case ByteCode.StackMapTableKey => getStackMapTableBytes(attribute)
        case ByteCode.SourceFileAttribute => getSourceFileBytes(attribute)
      })
  }

  def prefixWithIntLength(_bytes: () => Seq[Byte]): Seq[Byte] = {
    hexToBytes("cafebabe")
    val counterBefore = debugCounter
    val bytes = _bytes()
    if (counterBefore + bytes.length != debugCounter)
      System.out.append('a')
    Convert.intToBytes(bytes.length) ++ bytes
  }

  def getSourceFileBytes(sourceFile: MetaObject) = {
    shortToBytes(ByteCode.getSourceFileFileNameIndex(sourceFile))
  }

  def getVerificationInfoBytes(_type: MetaObject): Seq[Byte] = hexToBytes(_type.clazz match {
    case JavaTypes.IntTypeKey => "01"
    case JavaTypes.LongTypeKey => "02"
  })

  def getFrameByteCode(frame: MetaObject): Seq[Byte] = {
    val offset = ByteCode.getFrameOffset(frame)
    frame.clazz match {
      case ByteCode.SameFrameKey =>
        if (offset > 63)
          byteToBytes(251) ++ shortToBytes(offset)
        else
          byteToBytes(offset)
      case ByteCode.AppendFrame =>
        val localVerificationTypes = ByteCode.getAppendFrameTypes(frame)
        byteToBytes(252 + localVerificationTypes.length - 1) ++
          shortToBytes(offset) ++ localVerificationTypes.flatMap(info => getVerificationInfoBytes(info))
      case ByteCode.SameLocals1StackItem =>
        val _type = ByteCode.getSameLocals1StackItemType(frame)
        val code = 64 + offset
        if (code > 127)
          byteToBytes(247) ++ shortToBytes(offset) ++ getVerificationInfoBytes(_type)
        else {
          byteToBytes(code) ++ getVerificationInfoBytes(_type)
        }
    }
  }

  def getStackMapTableBytes(attribute: MetaObject): Seq[Byte] = {
    val entries = ByteCode.getStackMapTableEntries(attribute)
    shortToBytes(entries.length) ++ entries.flatMap(getFrameByteCode)
  }

  def getLineNumberTableBytes(attribute: MetaObject): Seq[Byte] = {
    val entries = ByteCode.getLineNumberTableEntries(attribute)
    shortToBytes(entries.length) ++
      entries.flatMap(getLineNumberTableEntryByteCode)
  }

  def getCodeAttributeBytes(attribute: MetaObject): Seq[Byte] = {
    val exceptionTable = ByteCode.getCodeExceptionTable(attribute)
    shortToBytes(ByteCode.getCodeMaxStack(attribute)) ++
      shortToBytes(ByteCode.getCodeMaxLocals(attribute)) ++
      prefixWithIntLength(() => ByteCode.getCodeInstructions(attribute).flatMap(getInstructionByteCode)) ++
      shortToBytes(exceptionTable.length) ++
      exceptionTable.flatMap(exception => getExceptionByteCode(exception)) ++
      getAttributesByteCode(ByteCode.getCodeAttributes(attribute))
  }

  def getLineNumberTableEntryByteCode(entry: LineNumberRef) =
    shortToBytes(entry.startProgramCounter) ++ shortToBytes(entry.lineNumber)

  def getInterfacesByteCode(clazz: MetaObject): Seq[Byte] = {
    val interfaces = ByteCode.getClassInterfaces(clazz)
    shortToBytes(interfaces.length) ++ interfaces.flatMap(interface => shortToBytes(interface))
  }

  def getFieldsByteCode(clazz: MetaObject) = {
    val fields = ByteCode.getClassFields(clazz)
    shortToBytes(fields.length) ++ fields.map(field => ???)
  }

  def getAccessFlagsByteCode(clazz: MetaObject): Seq[Byte] = {
    shortToBytes(accessFlags("super"))
  }

  def javaTypeToString(_type: MetaObject): String = _type.clazz match {
    case JavaTypes.VoidTypeKey => "V"
    case JavaTypes.IntTypeKey => "I"
    case JavaTypes.DoubleTypeKey => "D"
    case JavaTypes.LongTypeKey => "J"
    case JavaTypes.BooleanTypeKey => "Z"
    case ArrayTypeKey => s"[${javaTypeToString(JavaTypes.getArrayElementType(_type))}"
    case ObjectTypeKey => s"L${JavaTypes.getObjectTypeName(_type).right.get.parts.mkString("/")};"
  }

  def getConstantEntryByteCode(entry: Any): Seq[Byte] = {
    entry match {
      case metaEntry: MetaObject =>
        metaEntry.clazz match {
          case ByteCode.MethodRefKey =>
            byteToBytes(10) ++
              shortToBytes(ByteCode.getMethodRefClassRefIndex(metaEntry)) ++
              shortToBytes(ByteCode.getMethodRefMethodNameIndex(metaEntry))
          case ByteCode.FieldRef =>
            byteToBytes(9) ++
              shortToBytes(ByteCode.getFieldRefClassIndex(metaEntry)) ++
              shortToBytes(ByteCode.getFieldRefNameAndTypeIndex(metaEntry))
          case ByteCode.ClassRefKey =>
            byteToBytes(7) ++ shortToBytes(ByteCode.getClassRefName(metaEntry))
          case ByteCode.NameAndTypeKey =>
            byteToBytes(12) ++ shortToBytes(ByteCode.getNameAndTypeName(metaEntry)) ++
              shortToBytes(ByteCode.getNameAndTypeType(metaEntry))
          case ByteCode.MethodDescriptor =>
            val returnString = javaTypeToString(ByteCode.getMethodDescriptorReturnType(metaEntry))
            val parametersString = s"(${
              ByteCode.getMethodDescriptorParameters(metaEntry).map(javaTypeToString).mkString("")
            })"
            toUTF8ConstantEntry(parametersString + returnString)
          case JavaTypes.ObjectTypeKey => toUTF8ConstantEntry(javaTypeToString(metaEntry))
        }
      case CodeAttributeId => toUTF8ConstantEntry("Code")
      case StackMapTableId => toUTF8ConstantEntry("StackMapTable")
      case LineNumberTableId => toUTF8ConstantEntry("LineNumberTable")
      case SourceFileId => toUTF8ConstantEntry("SourceFile")
      case qualifiedName: QualifiedClassName => toUTF8ConstantEntry(qualifiedName.parts.mkString("/"))
      case utf8: String => toUTF8ConstantEntry(utf8)
    }
  }

  def hexToBytes(hex: String): Seq[Byte] = debugBytes(new BigInteger(hex, 16).toByteArray.takeRight(hex.length / 2))

  def toUTF8ConstantEntry(utf8: String): Seq[Byte] = {
    val bytes = utf8.toString.getBytes("UTF-8")
    byteToBytes(1) ++ shortToBytes(bytes.length) ++ debugBytes(bytes)
  }

  def byteToBytes(value: Int): Seq[Byte] = {
    debugBytes(Convert.intToBytes(value).drop(3))
  }

  def shortToBytes(short: Int): Seq[Byte] = {
    debugBytes(Convert.intToBytes(short).takeRight(2))
  }

  def shortToBytes(short: Short): Seq[Byte] = {
    ???
  }

  def intToBytes(int: Int): Seq[Byte] = {
    debugBytes(Convert.intToBytes(int))
  }

  def debugBytes(bytes: Seq[Byte]) = {
    val diff = bytes.length
    debugCounter = debugCounter + diff
    bytes
  }
}
