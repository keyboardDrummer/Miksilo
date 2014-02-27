package languages.bytecode

import transformation.MetaObject
import java.io.File
import akka.util.Convert
import java.nio.charset.Charset

object PrintByteCode {
  def print(byteCode: MetaObject) : String = {
    formatHexLikeClassFile(valueOf(getBytes(byteCode))).toLowerCase
  }

  def formatHexLikeClassFile(hex: String) : String = {
    hex.grouped(4).grouped(8).map(g => g.mkString(" ")).mkString("\n")
  }

  def valueOf(buf: Iterable[Byte]): String = buf.map("%02X" format _).mkString

  def getBytes(byteCode: MetaObject) : Seq[Byte] = {
    val clazz = byteCode
    var result = List[Byte]()

    result ++=  Convert.intToBytes(0xCAFEBABE)
    result ++= Convert.intToBytes(0x00000033)
    val constantPool = ByteCode.getConstantPool(clazz)
    val constantPoolSizePlusOne = shortToBytes(constantPool.length + 1)
    result ++= constantPoolSizePlusOne
    for(constantPoolEntry <- constantPool)
      result ++= getConstantEntryByteCode(constantPoolEntry)
    result ++= getAccessFlagsByteCode(clazz)
    result ++= shortToBytes(ByteCode.getClassNameIndex(clazz))
    result ++= shortToBytes(ByteCode.getParentIndex(clazz))
    result
  }

  def getAccessFlagsByteCode(clazz: MetaObject) : Seq[Byte] = {
    shortToBytes(0)
  }

  def getConstantEntryByteCode(entry: Any) = {
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
        }
      case utf8: String =>
        byteToBytes(1) ++ shortToBytes(utf8.length) ++ utf8.flatMap(c => byteToBytes(c))
    }
  }

  def byteToBytes(value: Char) : Seq[Byte] = {
    value.toString.getBytes("UTF-8")
  }

  def byteToBytes(value: Int) : Seq[Byte] = {
    Convert.intToBytes(value).drop(3)
  }

  def shortToBytes(short: Int) : Seq[Byte] = {
    Convert.intToBytes(short).drop(2)
  }

  def shortToBytes(short: Short) : Seq[Byte] = {
    ???
  }

}
