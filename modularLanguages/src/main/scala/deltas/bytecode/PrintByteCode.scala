package deltas.bytecode

import java.math.BigInteger

import core.language.Compilation
import core.language.node.Node
import deltas.bytecode.ByteCodeSkeleton._
import deltas.javac.classes.skeleton.QualifiedClassName

object PrintByteCode {
  def longToBytes(long: Long): LazyList[Byte] = LazyList.from(toByteArray(long))

  def toByteArray(value: Long): Array[Byte] = {
    // bugs when narrowing byte casts of long values occur.
    val result = new Array[Byte](8)
    var newValue = value
    for (i <- 7 to 0 by -1) {
      result(i) = (value & 0xffL).toByte
      newValue >>= 8
    }
    result
  }
  
  //TODO code uit deze classe naar byte code particles verplaatsen.
  val classAccessFlags = Map("super" -> 0x0020)

  val cafeBabeBytes: Seq[Byte] = intToBytes(0xCAFEBABE)

  val versionNumber: Seq[Byte] = intToBytes(0x00000033)

  def getBytes(compilation: Compilation, byteCode: Node): LazyList[Byte] = {

    val classFile = new ClassFile(byteCode)
    def getBytes(byteCode: Node): LazyList[Byte] = {
      var result = LazyList[Byte]()

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
      result ++= getAttributesByteCode(compilation, classFile.attributes)
      result
    }

    def getMethodsByteCode(classFile: ClassFile[Node]): Seq[Byte] = {
      val methods = classFile.methods
      shortToBytes(methods.length) ++ methods.flatMap(method => {
        ByteCodeSkeleton.getBytes(compilation, method)
      })
    }

    def getFieldsByteCode(classFile: ClassFile[Node]): Seq[Byte] = {
      val fields = classFile.fields
      PrintByteCode.shortToBytes(fields.length) ++ fields.flatMap(field => {
        ByteCodeSkeleton.getBytes(compilation, field)
      })
    }

    def getConstantEntryByteCode(entry: Any): Seq[Byte] = {
      entry match {
        case metaEntry: Node => ByteCodeSkeleton.getBytes(compilation, metaEntry)
        case qualifiedName: QualifiedClassName => toUTF8ConstantEntry(qualifiedName.parts.mkString("/")) //TODO is this still used?
        case utf8: String => toUTF8ConstantEntry(utf8)
      }
    }

    getBytes(byteCode)
  }

  def prefixWithIntLength(_bytes: () => Seq[Byte]): LazyList[Byte] = {
    val bytes = _bytes()
    LazyList.from(toByteArray(bytes.length)) ++ bytes
  }

  def toByteArray(value: Int) = Array[Byte]((value >> 24).toByte, (value >> 16).toByte, (value >> 8).toByte, value.toByte)
  
  def getInterfacesByteCode(shape: Node) = {
    val interfaces = shape.interfaceIndices
    shortToBytes(interfaces.length) ++ interfaces.flatMap(interface => shortToBytes(interface))
  }

  def getAccessFlagsByteCode(shape: Node): LazyList[Byte] = {
    shortToBytes(classAccessFlags("super"))
  }

  def hexToBytes(hex: String): LazyList[Byte] = LazyList.from(new BigInteger(hex, 16).toByteArray).takeRight(hex.length / 2)

  def toUTF8ConstantEntry(utf8: String): LazyList[Byte] = {
    val bytes = utf8.toString.getBytes("UTF-8")
    byteToBytes(1) ++ shortToBytes(bytes.length) ++ bytes
  }

  def getExceptionByteCode(exception: Node): LazyList[Byte] = ???

  def getAttributesByteCode(compilation: Compilation, attributes: Seq[Node]): Seq[Byte] = {

    def getAttributeByteCode(attribute: Node): Seq[Byte] = {
      shortToBytes(ByteCodeSkeleton.getAttributeNameIndex(attribute)) ++
        prefixWithIntLength(() => ByteCodeSkeleton.getBytes(compilation, attribute))
    }

    shortToBytes(attributes.length) ++ attributes.flatMap(attribute => getAttributeByteCode(attribute))
  }

  def hexToInt(hex: String): Int = new BigInteger(hex, 16).intValue()

  def byteToBytes(value: Int): LazyList[Byte] = {
    LazyList.from(toByteArray(value)).drop(3)
  }

  def shortToBytes(short: Int): LazyList[Byte] = {
    LazyList.from(toByteArray(short)).takeRight(2)
  }

  def intToBytes(int: Int): LazyList[Byte] = {
    LazyList.from(toByteArray(int))
  }

  def printBytes(bytes: LazyList[Byte]): String = {
    formatHexLikeClassFile(valueOf(bytes)).toLowerCase
  }

  def formatHexLikeClassFile(hex: String): String = {
    hex.grouped(4).grouped(8).map(g => g.mkString(" ")).mkString("\n")
  }

  def valueOf(buf: Iterable[Byte]): String = buf.map("%02X" format _).mkString
}
