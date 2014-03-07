package languages.bytecode

import transformation.MetaObject
import java.io.File
import akka.util.Convert
import java.nio.charset.{StandardCharsets, Charset}
import languages.bytecode.ByteCode._
import languages.javac.base.{JavaTypes, QualifiedClassName}
import java.math.BigInteger
import languages.bytecode.AppendFrame
import languages.javac.base.QualifiedClassName
import languages.bytecode.LineNumberRef
import languages.bytecode.SameFrame
import languages.javac.base.JavaTypes.{ObjectType, ArrayType}

object PrintByteCode {
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
    for (constantPoolEntry <- constantPool)
    {
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
      case ByteCode.IntegerIncrementKey => hexToBytes("84") ++
        byteToBytes(arguments(0)) ++
        byteToBytes(arguments(1))
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

  def prefixWithIntLength(_bytes: () => Seq[Byte]) : Seq[Byte] = {
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

  def getVerificationInfoBytes(info: Any): Seq[Byte] = hexToBytes("01") //Int -> 01

  def getStackMapTableBytes(attribute: MetaObject) : Seq[Byte] = {
    val entries = ByteCode.getStackMapTableEntries(attribute)
    shortToBytes(entries.length) ++
      entries.flatMap(entry => entry match {
        case same: SameFrame => byteToBytes(same.offsetDelta)
        case append: AppendFrame => byteToBytes(251 + append.localVerificationTypes.length) ++
          shortToBytes(append.offsetDelta) ++ append.localVerificationTypes.flatMap(info => getVerificationInfoBytes(info))
        case sameLocals1Stack: SameLocals1StackItem =>
          byteToBytes(64 + sameLocals1Stack.offsetDelta) ++ getVerificationInfoBytes(sameLocals1Stack._type)
      })
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

  val accessFlags: Map[String, Int] = Map("super" -> 0x0020)

  def getAccessFlagsByteCode(clazz: MetaObject): Seq[Byte] = {
    shortToBytes(accessFlags("super"))
  }

  def javaTypeToString(_type: Any): String = _type match {
    case JavaTypes.VoidType => "V"
    case JavaTypes.IntegerType => "I"
    case JavaTypes.DoubleType => "D"
    case JavaTypes.LongType => "J"
    case meta: MetaObject => meta.clazz match {
      case ArrayType => s"[${javaTypeToString(JavaTypes.getArrayElementType(meta))}"
      case ObjectType => s"L${JavaTypes.getObjectTypeName(meta).right.get.parts.mkString("/")};"
    }
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
              shortToBytes(ByteCode.getFieldRefNameIndex(metaEntry))
          case ByteCode.ClassRefKey =>
            byteToBytes(7) ++ shortToBytes(ByteCode.getClassRefName(metaEntry))
          case ByteCode.NameAndTypeKey =>
            byteToBytes(12) ++ shortToBytes(ByteCode.getNameAndTypeName(metaEntry)) ++
              shortToBytes(ByteCode.getNameAndTypeType(metaEntry))
          case ByteCode.MethodDescriptor =>
            val returnString = javaTypeToString(ByteCode.getMethodDescriptorReturnType(metaEntry))
            val parametersString = s"(${
              ByteCode.getMethodDescriptorParameters(metaEntry)
                .map(javaTypeToString).mkString("")
            })"
            toUTF8ConstantEntry(parametersString + returnString)
          case JavaTypes.ObjectType => toUTF8ConstantEntry(javaTypeToString(metaEntry))
        }
      case CodeAttributeId => toUTF8ConstantEntry("Code")
      case StackMapTableId => toUTF8ConstantEntry("StackMapTable")
      case LineNumberTableId => toUTF8ConstantEntry("LineNumberTable")
      case SourceFileId => toUTF8ConstantEntry("SourceFile")
      case qualifiedName: QualifiedClassName => toUTF8ConstantEntry(qualifiedName.parts.mkString("/"))
      case utf8: String => toUTF8ConstantEntry(utf8)
    }
  }

  def hexToBytes(hex: String): Seq[Byte] = debugBytes(new BigInteger(hex, 16).toByteArray().takeRight(hex.length/2))

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

  var debugCounter: Int = 0
  def debugBytes(bytes: Seq[Byte]) = {
    val diff = bytes.length
    debugCounter = debugCounter + diff
    bytes
  }
}
