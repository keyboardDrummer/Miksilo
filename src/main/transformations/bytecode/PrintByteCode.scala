package transformations.bytecode

import java.math.BigInteger

import akka.util.Convert
import core.particles.CompilationState
import core.particles.node.Node
import transformations.javac.classes.skeleton.QualifiedClassName

object PrintByteCode {
  def longToBytes(long: Long): scala.Seq[Byte] = Convert.longToBytes(long)

  //TODO code uit deze classe naar byte code particles verplaatsen.
  val classAccessFlags: Map[String, Int] = Map("super" -> 0x0020)
  var debugCounter: Int = 0

  val cafeBabeBytes: Seq[Byte] = intToBytes(0xCAFEBABE)

  val versionNumber: Seq[Byte] = intToBytes(0x00000033)

  def getBytes(byteCode: Node, state: CompilationState): Seq[Byte] = {

    val clazz = byteCode
    def getBytes(byteCode: Node): Seq[Byte] = {
      var result = List[Byte]()

      result ++= cafeBabeBytes
      result ++= versionNumber
      val constantPool = ByteCodeSkeleton.getConstantPool(clazz).constants
      val constantPoolItemCountPlusOne = shortToBytes(constantPool.length + 1)
      result ++= constantPoolItemCountPlusOne
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

    def getMethodsByteCode(clazz: Node): Seq[Byte] = {
      val methods = ByteCodeSkeleton.getMethods(clazz)
      shortToBytes(methods.length) ++ methods.flatMap(method => {
        ByteCodeSkeleton.getState(state).getBytes(method.clazz)(method)
      })
    }

    def getFieldsByteCode(clazz: Node): Seq[Byte] = {
      val fields = ByteCodeSkeleton.getClassFields(clazz)
      PrintByteCode.shortToBytes(fields.length) ++ fields.flatMap(field => {
        ByteCodeSkeleton.getState(state).getBytes(field.clazz)(field)
      })
    }

    def getConstantEntryByteCode(entry: Any): Seq[Byte] = {
      entry match {
        case metaEntry: Node => ByteCodeSkeleton.getState(state).getBytes(metaEntry.clazz)(metaEntry)
        case qualifiedName: QualifiedClassName => toUTF8ConstantEntry(qualifiedName.parts.mkString("/"))
        case utf8: String => toUTF8ConstantEntry(utf8)
      }
    }

    getBytes(byteCode)
  }

  def prefixWithIntLength(_bytes: () => Seq[Byte]): Seq[Byte] = {
    hexToBytes("cafebabe")
    val counterBefore = debugCounter
    val bytes = _bytes()
    if (counterBefore + bytes.length != debugCounter)
      System.out.append('a')
    Convert.intToBytes(bytes.length) ++ bytes
  }

  def getInterfacesByteCode(clazz: Node): Seq[Byte] = {
    val interfaces = ByteCodeSkeleton.getClassInterfaces(clazz)
    shortToBytes(interfaces.length) ++ interfaces.flatMap(interface => shortToBytes(interface))
  }

  def getAccessFlagsByteCode(clazz: Node): Seq[Byte] = {
    shortToBytes(classAccessFlags("super"))
  }

  def hexToBytes(hex: String): Seq[Byte] = debugBytes(new BigInteger(hex, 16).toByteArray.takeRight(hex.length / 2))

  def toUTF8ConstantEntry(utf8: String): Seq[Byte] = {
    val bytes = utf8.toString.getBytes("UTF-8")
    byteToBytes(1) ++ shortToBytes(bytes.length) ++ debugBytes(bytes)
  }

  def getExceptionByteCode(exception: Node): Seq[Byte] = ???

  def getAttributesByteCode(state: CompilationState, attributes: Seq[Node]): Seq[Byte] = {

    def getAttributeByteCode(attribute: Node): Seq[Byte] = {
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
}
