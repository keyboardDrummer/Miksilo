package transformations.bytecode

import java.math.BigInteger

import akka.util.Convert
import core.transformation.sillyCodePieces.ParticleWithPhase
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.bytecode.ByteCodeSkeleton.State
import transformations.javac.classes.QualifiedClassName
import transformations.types.{ObjectTypeC, TypeC}

object PrintByteCode extends ParticleWithPhase { //TODO code uit deze classe naar byte code particles verplaatsen.
  val accessFlags: Map[String, Int] = Map("super" -> 0x0020)
  var debugCounter: Int = 0

  def getState(state: TransformationState) = state.data.getOrElseUpdate(this, new State()).asInstanceOf[State]

  def prefixWithIntLength(_bytes: () => Seq[Byte]): Seq[Byte] = {
    hexToBytes("cafebabe")
    val counterBefore = debugCounter
    val bytes = _bytes()
    if (counterBefore + bytes.length != debugCounter)
      System.out.append('a')
    Convert.intToBytes(bytes.length) ++ bytes
  }

  def getInterfacesByteCode(clazz: MetaObject): Seq[Byte] = {
    val interfaces = ByteCodeSkeleton.getClassInterfaces(clazz)
    shortToBytes(interfaces.length) ++ interfaces.flatMap(interface => shortToBytes(interface))
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

  def getAttributesByteCode(state: TransformationState, attributes: Seq[MetaObject]): Seq[Byte] = {

    def getAttributeByteCode(attribute: MetaObject): Seq[Byte] = {
      shortToBytes(ByteCodeSkeleton.getAttributeNameIndex(attribute)) ++
        prefixWithIntLength(() => ByteCodeSkeleton.getState(state).getBytes(attribute.clazz)(attribute))
    }

    shortToBytes(attributes.length) ++ attributes.flatMap(attribute => getAttributeByteCode(attribute))
  }

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

    val clazz = byteCode
    def getBytes(byteCode: MetaObject): Seq[Byte] = {
      var result = List[Byte]()

      result ++= intToBytes(0xCAFEBABE)
      result ++= intToBytes(0x00000033)
      val constantPool = ByteCodeSkeleton.getConstantPool(clazz).constants
      val constantPoolSizePlusOne = shortToBytes(constantPool.length + 1)
      result ++= constantPoolSizePlusOne
      for (constantPoolEntry <- constantPool) {
        result ++= getConstantEntryByteCode(constantPoolEntry)
      }
      result ++= getAccessFlagsByteCode(clazz)
      result ++= shortToBytes(ByteCodeSkeleton.getClassNameIndex(clazz))
      result ++= shortToBytes(ByteCodeSkeleton.getParentIndex(clazz))
      result ++= getInterfacesByteCode(clazz)
      result ++= ByteCodeField.getFieldsByteCode(clazz, state)
      result ++= ByteCodeMethodInfo.getMethodsByteCode(clazz, state)
      result ++= getAttributesByteCode(state, ByteCodeSkeleton.getClassAttributes(clazz))
      result
    }

    def getConstantEntryByteCode(entry: Any): Seq[Byte] = {
      entry match {
        case metaEntry: MetaObject =>
          metaEntry.clazz match {
            case ObjectTypeC.ObjectTypeKey => toUTF8ConstantEntry(javaTypeToString(metaEntry))
            case _ => ByteCodeSkeleton.getState(state).getBytes(metaEntry.clazz)(metaEntry)
          }
        case qualifiedName: QualifiedClassName => toUTF8ConstantEntry(qualifiedName.parts.mkString("/"))
        case utf8: String => toUTF8ConstantEntry(utf8)
      }
    }

    def javaTypeToString(_type: MetaObject): String = TypeC.getByteCodeString(state)(_type)

    getBytes(byteCode)
  }

  override def transform(program: MetaObject, state: TransformationState): Unit = {

  }

  override def dependencies: Set[Contract] = Set.empty

}
