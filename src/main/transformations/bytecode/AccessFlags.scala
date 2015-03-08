package transformations.bytecode

import core.particles.MetaObject
import transformations.bytecode.PrintByteCode._

trait AccessFlags {
  trait MethodAccessFlag

  object PublicAccess extends MethodAccessFlag

  object StaticAccess extends MethodAccessFlag

  object PrivateAccess extends MethodAccessFlag

  object AccessFlagsKey

  private val accessCodesToByteCode = Map(
    PublicAccess -> "0001",
    StaticAccess -> "0008",
    PrivateAccess -> "0002").mapValues(s => hexToInt(s))

  def getAccessFlagsByteCode(field: MetaObject): Seq[Byte] = {
    getAccessFlagsByteCode(getAccessFlags(field))
  }

  private def getAccessFlagsByteCode(accessFlags: Set[MethodAccessFlag]): Seq[Byte] = {
    shortToBytes(accessFlags.map(flag => accessCodesToByteCode(flag)).sum)
  }

  private def getAccessFlags(_object: MetaObject): Set[MethodAccessFlag] = {
    _object(AccessFlagsKey).asInstanceOf[Set[MethodAccessFlag]]
  }
}
