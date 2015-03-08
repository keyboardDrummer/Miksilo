package transformations.bytecode

import java.math.BigInteger

import akka.util.Convert
import core.particles.{MetaObject, CompilationState}
import transformations.javac.classes.QualifiedClassName
import transformations.types.TypeSkeleton

object PrintByteCode { //TODO code uit deze classe naar byte code particles verplaatsen.
  val classAccessFlags: Map[String, Int] = Map("super" -> 0x0020)
  var debugCounter: Int = 0

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
    shortToBytes(classAccessFlags("super"))
  }

  def hexToBytes(hex: String): Seq[Byte] = debugBytes(new BigInteger(hex, 16).toByteArray.takeRight(hex.length / 2))

  def toUTF8ConstantEntry(utf8: String): Seq[Byte] = {
    val bytes = utf8.toString.getBytes("UTF-8")
    byteToBytes(1) ++ shortToBytes(bytes.length) ++ debugBytes(bytes)
  }

  def getExceptionByteCode(exception: MetaObject): Seq[Byte] = ???

  def getAttributesByteCode(state: CompilationState, attributes: Seq[MetaObject]): Seq[Byte] = {

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

  def getBytes(byteCode: MetaObject, state: CompilationState): Seq[Byte] = {

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
      result ++= getFieldsByteCode(clazz)
      result ++= getMethodsByteCode(clazz)
      result ++= getAttributesByteCode(state, ByteCodeSkeleton.getClassAttributes(clazz))
      result
    }

    def getMethodsByteCode(clazz: MetaObject): Seq[Byte] = {
      val methods = ByteCodeSkeleton.getMethods(clazz)
      shortToBytes(methods.length) ++ methods.flatMap(method => {
        ByteCodeSkeleton.getState(state).getBytes(method.clazz)(method)
      })
    }

    def getFieldsByteCode(clazz: MetaObject): Seq[Byte] = {
      val fields = ByteCodeSkeleton.getClassFields(clazz)
      PrintByteCode.shortToBytes(fields.length) ++ fields.flatMap(field => {
        ByteCodeSkeleton.getState(state).getBytes(field.clazz)(field)
      })
    }

    def getConstantEntryByteCode(entry: Any): Seq[Byte] = {
      entry match {
        case metaEntry: MetaObject => ByteCodeSkeleton.getState(state).getBytes(metaEntry.clazz)(metaEntry)
        case qualifiedName: QualifiedClassName => toUTF8ConstantEntry(qualifiedName.parts.mkString("/"))
        case utf8: String => toUTF8ConstantEntry(utf8)
      }
    }

    def javaTypeToString(_type: MetaObject): String = TypeSkeleton.getByteCodeString(state)(_type)

    getBytes(byteCode)
  }
}
