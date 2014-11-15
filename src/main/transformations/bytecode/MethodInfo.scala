package transformations.bytecode

trait MethodInfo {
  trait MethodAccessFlag

  object PublicAccess extends MethodAccessFlag

  object StaticAccess extends MethodAccessFlag

  object PrivateAccess extends MethodAccessFlag

  object MethodInfoKey

  object MethodNameIndex

  object MethodDescriptorIndex

  object MethodAnnotations

  object MethodAccessFlags

  def methodInfo(nameIndex: Int, descriptorIndex: Int, annotations: Seq[MetaObject], flags: Set[MethodAccessFlag] = Set()) =
    new MetaObject(MethodInfoKey) {
      data.put(MethodAnnotations, annotations)
      data.put(MethodNameIndex, nameIndex)
      data.put(MethodDescriptorIndex, descriptorIndex)
      data.put(MethodAccessFlags, flags)
    }

  def getMethodAccessFlags(method: MetaObject) = method(MethodAccessFlags).asInstanceOf[Set[MethodAccessFlag]]

  def getMethodNameIndex(methodInfo: MetaObject) = methodInfo(MethodNameIndex).asInstanceOf[Int]

  def getMethodDescriptorIndex(methodInfo: MetaObject) = methodInfo(MethodDescriptorIndex).asInstanceOf[Int]
}
