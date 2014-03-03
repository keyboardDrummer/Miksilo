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

    result ++= Convert.intToBytes(0xCAFEBABE)
    result ++= Convert.intToBytes(0x00000033)
    val constantPool = ByteCode.getConstantPool(clazz)
    val constantPoolSizePlusOne = shortToBytes(constantPool.length + 1)
    result ++= constantPoolSizePlusOne
    for (constantPoolEntry <- constantPool)
      result ++= getConstantEntryByteCode(constantPoolEntry)
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

  def aaaaBytes = hexToBytes("aaaa")

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
    val innerBytes : Seq[Byte] = attribute.clazz match {
      case ByteCode.CodeKey =>
        getCodeAttributeBytes(attribute)
      case ByteCode.LineNumberTableKey =>
        getLineNumberTableBytes(attribute)
      case ByteCode.StackMapTableKey => getStackMapTableBytes(attribute)
      case ByteCode.SourceFileAttribute => getSourceFileBytes(attribute)
    }
    val bytes = shortToBytes(ByteCode.getAttributeNameIndex(attribute)) ++
      intToBytes(innerBytes.length) ++
      innerBytes
    bytes
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
      })
  }

  def getLineNumberTableBytes(attribute: MetaObject): Seq[Byte] = {
    val entries = ByteCode.getLineNumberTableEntries(attribute)
    shortToBytes(entries.length) ++
      entries.flatMap(getLineNumberTableEntryByteCode)
  }

  def getCodeAttributeBytes(attribute: MetaObject): Seq[Byte] = {
    val exceptionTable = ByteCode.getCodeExceptionTable(attribute)
    val code: Seq[Byte] = ByteCode.getCodeInstructions(attribute).flatMap(getInstructionByteCode)
    val codeLength = code.length
    val innerBytes =
      shortToBytes(ByteCode.getCodeMaxStack(attribute)) ++
        shortToBytes(ByteCode.getCodeMaxLocals(attribute)) ++
        intToBytes(codeLength) ++
        code ++
        shortToBytes(exceptionTable.length) ++
        exceptionTable.flatMap(exception => getExceptionByteCode(exception)) ++
        getAttributesByteCode(ByteCode.getCodeAttributes(attribute))
    innerBytes
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
    case JavaTypes.LongType => "L"
  }

  def getConstantEntryByteCode(entry: Any): Seq[Byte] = {
    entry match {
      case metaEntry: MetaObject =>
        metaEntry.clazz match {
          case ByteCode.MethodRefKey =>
            byteToBytes(10) ++
              shortToBytes(ByteCode.getMethodRefClassRefIndex(metaEntry)) ++
              shortToBytes(ByteCode.getMethodRefMethodNameIndex(metaEntry))
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
        }
      case CodeAttributeId => toUTF8ConstantEntry("Code")
      case StackMapTableId => toUTF8ConstantEntry("StackMapTable")
      case LineNumberTableId => toUTF8ConstantEntry("LineNumberTable")
      case SourceFileId => toUTF8ConstantEntry("SourceFile")
      case qualifiedName: QualifiedClassName => toUTF8ConstantEntry(qualifiedName.parts.mkString("/"))
      case utf8: String => toUTF8ConstantEntry(utf8)
    }
  }

  def hexToBytes(hex: String): Seq[Byte] = new BigInteger(hex, 16).toByteArray().takeRight(hex.length/2)

  def toUTF8ConstantEntry(utf8: String): Seq[Byte] = {
    val bytes = utf8.toString.getBytes("UTF-8")
    byteToBytes(1) ++ shortToBytes(bytes.length) ++ bytes
  }

  def byteToBytes(value: Int): Seq[Byte] = {
    Convert.intToBytes(value).drop(3)
  }

  def shortToBytes(short: Int): Seq[Byte] = {
    Convert.intToBytes(short).takeRight(2)
  }

  def shortToBytes(short: Short): Seq[Byte] = {
    ???
  }

  def intToBytes(int: Int): Seq[Byte] = {
    Convert.intToBytes(int)
  }

}
