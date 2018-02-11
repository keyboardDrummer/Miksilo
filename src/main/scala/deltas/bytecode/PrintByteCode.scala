package deltas.bytecode

import java.math.BigInteger

import com.google.common.primitives.{Ints, Longs}
import core.deltas.node.Node
import core.language.Language
import deltas.javac.classes.skeleton.QualifiedClassName
import deltas.bytecode.ByteCodeSkeleton._

object PrintByteCode {
  def longToBytes(long: Long): scala.Seq[Byte] = Longs.toByteArray(long)

  //TODO code uit deze classe naar byte code particles verplaatsen.
  val classAccessFlags: Map[String, Int] = Map("super" -> 0x0020)
  var debugCounter: Int = 0

  val cafeBabeBytes: Seq[Byte] = intToBytes(0xCAFEBABE)

  val versionNumber: Seq[Byte] = intToBytes(0x00000033)

  def getBytes(byteCode: Node, language: Language): Seq[Byte] = {

    val classFile = new ClassFile(byteCode)
    def getBytes(byteCode: Node): Seq[Byte] = {
      var result = List[Byte]()

      result ++= cafeBabeBytes
      result ++= versionNumber
      val constantPool = classFile.constantPool.constants
      val constantPoolItemCountPlusOne = shortToBytes(constantPool.length + 1)
      result ++= constantPoolItemCountPlusOne
      for (constantPoolEntry <- constantPool) {
        result ++= getConstantEntryByteCode(constantPoolEntry)
      }
      result ++= getAccessFlagsByteCode(classFile)
      result ++= shortToBytes(classFile.classInfoIndex)
      result ++= shortToBytes(classFile.parentIndex)
      result ++= getInterfacesByteCode(classFile)
      result ++= getFieldsByteCode(classFile)
      result ++= getMethodsByteCode(classFile)
      result ++= getAttributesByteCode(language, classFile.attributes)
      result
    }

    def getMethodsByteCode(classFile: ClassFile[Node]): Seq[Byte] = {
      val methods = classFile.methods
      shortToBytes(methods.length) ++ methods.flatMap(method => {
        ByteCodeSkeleton.getRegistry(language).getBytes(method.shape)(method)
      })
    }

    def getFieldsByteCode(classFile: ClassFile[Node]): Seq[Byte] = {
      val fields = classFile.fields
      PrintByteCode.shortToBytes(fields.length) ++ fields.flatMap(field => {
        ByteCodeSkeleton.getRegistry(language).getBytes(field.shape)(field)
      })
    }

    def getConstantEntryByteCode(entry: Any): Seq[Byte] = {
      entry match {
        case metaEntry: Node => ByteCodeSkeleton.getRegistry(language).getBytes(metaEntry.shape)(metaEntry)
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
    Ints.toByteArray(bytes.length) ++ bytes
  }

  def getInterfacesByteCode(shape: Node): Seq[Byte] = {
    val interfaces = shape.interfaceIndices
    shortToBytes(interfaces.length) ++ interfaces.flatMap(interface => shortToBytes(interface))
  }

  def getAccessFlagsByteCode(shape: Node): Seq[Byte] = {
    shortToBytes(classAccessFlags("super"))
  }

  def hexToBytes(hex: String): Seq[Byte] = debugBytes(new BigInteger(hex, 16).toByteArray.takeRight(hex.length / 2))

  def toUTF8ConstantEntry(utf8: String): Seq[Byte] = {
    val bytes = utf8.toString.getBytes("UTF-8")
    byteToBytes(1) ++ shortToBytes(bytes.length) ++ debugBytes(bytes)
  }

  def getExceptionByteCode(exception: Node): Seq[Byte] = ???

  def getAttributesByteCode(state: Language, attributes: Seq[Node]): Seq[Byte] = {

    def getAttributeByteCode(attribute: Node): Seq[Byte] = {
      shortToBytes(ByteCodeSkeleton.getAttributeNameIndex(attribute)) ++
        prefixWithIntLength(() => ByteCodeSkeleton.getRegistry(state).getBytes(attribute.shape)(attribute))
    }

    shortToBytes(attributes.length) ++ attributes.flatMap(attribute => getAttributeByteCode(attribute))
  }

  def hexToInt(hex: String): Int = new BigInteger(hex, 16).intValue()

  def byteToBytes(value: Int): Seq[Byte] = {
    debugBytes(Ints.toByteArray(value).drop(3))
  }

  def shortToBytes(short: Int): Seq[Byte] = {
    debugBytes(Ints.toByteArray(short).takeRight(2))
  }

  def intToBytes(int: Int): Seq[Byte] = {
    debugBytes(Ints.toByteArray(int))
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
