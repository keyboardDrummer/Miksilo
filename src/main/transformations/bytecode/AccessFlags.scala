package transformations.bytecode

trait AccessFlags {
  trait MethodAccessFlag

  object PublicAccess extends MethodAccessFlag

  object StaticAccess extends MethodAccessFlag

  object PrivateAccess extends MethodAccessFlag

  object AccessFlagsKey
}
