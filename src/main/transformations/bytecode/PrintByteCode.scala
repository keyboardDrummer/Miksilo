package transformations.bytecode

import java.math.BigInteger

import akka.util.Convert
import core.transformation.sillyCodePieces.ProgramTransformation
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.bytecode.ByteCodeSkeleton._
import transformations.javac.base.model.QualifiedClassName
import transformations.javac.types.{ObjectTypeC, TypeC}

import scala.collection.mutable

object PrintByteCode extends ProgramTransformation {
  val accessFlags: Map[String, Int] = Map("super" -> 0x0020)
  var debugCounter: Int = 0

  def getState(state: TransformationState) = state.data.getOrElseUpdate(this, new State()).asInstanceOf[State]

  def getBytesRegistry(state: TransformationState) = getState(state).getBytesRegistry

  def prefixWithIntLength(_bytes: () => Seq[Byte]): Seq[Byte] = {
    hexToBytes("cafebabe")
    val counterBefore = debugCounter
    val bytes = _bytes()
    if (counterBefore + bytes.length != debugCounter)
      System.out.append('a')
    Convert.intToBytes(bytes.length) ++ bytes
  }

  def getSourceFileBytes(sourceFile: MetaObject) = {
    shortToBytes(ByteCodeSkeleton.getSourceFileFileNameIndex(sourceFile))
  }

  def getLineNumberTableBytes(attribute: MetaObject): Seq[Byte] = {
    val entries = ByteCodeSkeleton.getLineNumberTableEntries(attribute)
    shortToBytes(entries.length) ++
      entries.flatMap(getLineNumberTableEntryByteCode)
  }

  def getLineNumberTableEntryByteCode(entry: LineNumberRef) =
    shortToBytes(entry.startProgramCounter) ++ shortToBytes(entry.lineNumber)

  def getInterfacesByteCode(clazz: MetaObject): Seq[Byte] = {
    val interfaces = ByteCodeSkeleton.getClassInterfaces(clazz)
    shortToBytes(interfaces.length) ++ interfaces.flatMap(interface => shortToBytes(interface))
  }

  def getFieldsByteCode(clazz: MetaObject) = {
    val fields = ByteCodeSkeleton.getClassFields(clazz)
    shortToBytes(fields.length) ++ fields.map(field => ???)
  }

  def getAccessFlagsByteCode(clazz: MetaObject): Seq[Byte] = {
    shortToBytes(accessFlags("super"))
  }


  def hexToBytes(hex: String): Seq[Byte] = debugBytes(new BigInteger(hex, 16).toByteArray.takeRight(hex.length / 2))

  def toUTF8ConstantEntry(utf8: String): Seq[Byte] = {
    val bytes = utf8.toString.getBytes("UTF-8")
    byteToBytes(1) ++ shortToBytes(bytes.length) ++ debugBytes(bytes)
  }

  def getExceptionByteCode(exception: MetaObject): Seq[Byte] = ???

  def hexToInt(hex: String): Int = new BigInteger(hex, 16).intValue()

  def byteToBytes(value: Int): Seq[Byte] = {
    debugBytes(Convert.intToBytes(value).drop(3))
  }

  def shortToBytes(short: Int): Seq[Byte] = {
    debugBytes(Convert.intToBytes(short).takeRight(2))
  }

  def intToBytes(int: Int): Seq[Byte] = {
    debugBytes(Convert.intToBytes(int))
  }

  def debugBytes(bytes: Seq[Byte]) = {
    val diff = bytes.length
    debugCounter = debugCounter + diff
    bytes
  }

  def printBytes(bytes: Seq[Byte]): String = {
    formatHexLikeClassFile(valueOf(bytes)).toLowerCase
  }

  def formatHexLikeClassFile(hex: String): String = {
    hex.grouped(4).grouped(8).map(g => g.mkString(" ")).mkString("\n")
  }

  def valueOf(buf: Iterable[Byte]): String = buf.map("%02X" format _).mkString

  def getBytes(byteCode: MetaObject, state: TransformationState): Seq[Byte] = {

    def getBytes(byteCode: MetaObject): Seq[Byte] = {
      val clazz = byteCode
      var result = List[Byte]()

      result ++= intToBytes(0xCAFEBABE)
      result ++= intToBytes(0x00000033)
      val constantPool = ByteCodeSkeleton.getConstantPool(clazz)
      val constantPoolSizePlusOne = shortToBytes(constantPool.length + 1)
      result ++= constantPoolSizePlusOne
      for (constantPoolEntry <- constantPool) {
        result ++= getConstantEntryByteCode(constantPoolEntry)
      }
      result ++= getAccessFlagsByteCode(clazz)
      result ++= shortToBytes(ByteCodeSkeleton.getClassNameIndex(clazz))
      result ++= shortToBytes(ByteCodeSkeleton.getParentIndex(clazz))
      result ++= getInterfacesByteCode(clazz)
      result ++= getFieldsByteCode(clazz)
      result ++= getMethodsByteCode(clazz)
      result ++= getAttributesByteCode(ByteCodeSkeleton.getClassAttributes(clazz))
      result
    }

    def getConstantEntryByteCode(entry: Any): Seq[Byte] = {
      entry match {
        case metaEntry: MetaObject =>
          metaEntry.clazz match {
            case ByteCodeSkeleton.MethodRefKey =>
              byteToBytes(10) ++
                shortToBytes(ByteCodeSkeleton.getMethodRefClassRefIndex(metaEntry)) ++
                shortToBytes(ByteCodeSkeleton.getMethodRefMethodNameIndex(metaEntry))
            case ByteCodeSkeleton.FieldRef =>
              byteToBytes(9) ++
                shortToBytes(ByteCodeSkeleton.getFieldRefClassIndex(metaEntry)) ++
                shortToBytes(ByteCodeSkeleton.getFieldRefNameAndTypeIndex(metaEntry))
            case ByteCodeSkeleton.ClassRefKey =>
              byteToBytes(7) ++ shortToBytes(ByteCodeSkeleton.getClassRefName(metaEntry))
            case ByteCodeSkeleton.NameAndTypeKey =>
              byteToBytes(12) ++ shortToBytes(ByteCodeSkeleton.getNameAndTypeName(metaEntry)) ++
                shortToBytes(ByteCodeSkeleton.getNameAndTypeType(metaEntry))
            case ByteCodeSkeleton.MethodDescriptor =>
              val returnString = javaTypeToString(ByteCodeSkeleton.getMethodDescriptorReturnType(metaEntry))
              val parametersString = s"(${
                ByteCodeSkeleton.getMethodDescriptorParameters(metaEntry).map(javaTypeToString).mkString("")
              })"
              toUTF8ConstantEntry(parametersString + returnString)
            case ObjectTypeC.ObjectTypeKey => toUTF8ConstantEntry(javaTypeToString(metaEntry))
          }
        case CodeAttributeId => toUTF8ConstantEntry("Code")
        case StackMapTableId => toUTF8ConstantEntry("StackMapTable")
        case LineNumberTableId => toUTF8ConstantEntry("LineNumberTable")
        case SourceFileId => toUTF8ConstantEntry("SourceFile")
        case qualifiedName: QualifiedClassName => toUTF8ConstantEntry(qualifiedName.parts.mkString("/"))
        case utf8: String => toUTF8ConstantEntry(utf8)
      }
    }

    def getFrameByteCode(frame: MetaObject): Seq[Byte] = {
      val offset = ByteCodeSkeleton.getFrameOffset(frame)
      frame.clazz match {
        case ByteCodeSkeleton.SameFrameKey =>
          if (offset > 63)
            byteToBytes(251) ++ shortToBytes(offset)
          else
            byteToBytes(offset)
        case ByteCodeSkeleton.AppendFrame =>
          val localVerificationTypes = ByteCodeSkeleton.getAppendFrameTypes(frame)
          byteToBytes(252 + localVerificationTypes.length - 1) ++
            shortToBytes(offset) ++ localVerificationTypes.flatMap(info => TypeC.getVerificationInfoBytes(info, state))
        case ByteCodeSkeleton.SameLocals1StackItem =>
          val _type = ByteCodeSkeleton.getSameLocals1StackItemType(frame)
          val code = 64 + offset
          if (code > 127)
            byteToBytes(247) ++ shortToBytes(offset) ++ TypeC.getVerificationInfoBytes(_type, state)
          else {
            byteToBytes(code) ++ TypeC.getVerificationInfoBytes(_type, state)
          }
      }
    }

    def getStackMapTableBytes(attribute: MetaObject): Seq[Byte] = {
      val entries = ByteCodeSkeleton.getStackMapTableEntries(attribute)
      shortToBytes(entries.length) ++ entries.flatMap(getFrameByteCode)
    }

    def javaTypeToString(_type: MetaObject): String = TypeC.getByteCodeString(state)(_type)

    def getMethodsByteCode(clazz: MetaObject): Seq[Byte] = {
      val methods = ByteCodeSkeleton.getMethods(clazz)
      shortToBytes(methods.length) ++ methods.flatMap(method => getMethodByteCode(method))
    }

    def getMethodByteCode(methodInfo: MetaObject) = {
      val accessCodes = Map(
        ByteCodeSkeleton.PublicAccess -> "0001",
        ByteCodeSkeleton.StaticAccess -> "0008",
        ByteCodeSkeleton.PrivateAccess -> "0002").mapValues(s => hexToInt(s))
      shortToBytes(ByteCodeSkeleton.getMethodAccessFlags(methodInfo).map(flag => accessCodes(flag)).sum) ++
        shortToBytes(ByteCodeSkeleton.getMethodNameIndex(methodInfo)) ++
        shortToBytes(ByteCodeSkeleton.getMethodDescriptorIndex(methodInfo)) ++
        getAttributesByteCode(ByteCodeSkeleton.getMethodAttributes(methodInfo))
    }

    def getAttributesByteCode(attributes: Seq[MetaObject]) = {
      shortToBytes(attributes.length) ++ attributes.flatMap(attribute => getAttributeByteCode(attribute))
    }


    def getAttributeByteCode(attribute: MetaObject): Seq[Byte] = {
      shortToBytes(ByteCodeSkeleton.getAttributeNameIndex(attribute)) ++
        prefixWithIntLength(() => attribute.clazz match {
          case ByteCodeSkeleton.CodeKey =>
            getCodeAttributeBytes(attribute)
          case ByteCodeSkeleton.LineNumberTableKey =>
            getLineNumberTableBytes(attribute)
          case ByteCodeSkeleton.StackMapTableKey => getStackMapTableBytes(attribute)
          case ByteCodeSkeleton.SourceFileAttribute => getSourceFileBytes(attribute)
        })
    }

    def getCodeAttributeBytes(attribute: MetaObject): Seq[Byte] = {
      val exceptionTable = ByteCodeSkeleton.getCodeExceptionTable(attribute)
      shortToBytes(ByteCodeSkeleton.getCodeMaxStack(attribute)) ++
        shortToBytes(ByteCodeSkeleton.getCodeMaxLocals(attribute)) ++
        prefixWithIntLength(() => ByteCodeSkeleton.getCodeInstructions(attribute).flatMap(getInstructionByteCode)) ++
        shortToBytes(exceptionTable.length) ++
        exceptionTable.flatMap(exception => getExceptionByteCode(exception)) ++
        getAttributesByteCode(ByteCodeSkeleton.getCodeAttributes(attribute))
    }

    def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
      getBytesRegistry(state)(instruction.clazz)(instruction)
    }

    getBytes(byteCode)
  }

  override def transform(program: MetaObject, state: TransformationState): Unit = {


  }

  override def dependencies: Set[Contract] = Set.empty

  class State {
    var getBytesRegistry = new mutable.HashMap[Any, MetaObject => Seq[Byte]]
  }

}
