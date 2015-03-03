package transformations.bytecode

import core.transformation.MetaObject
import core.transformation.sillyCodePieces.GrammarTransformation



trait MethodInfo extends GrammarTransformation with AccessFlags {

  object MethodInfoKey

  object MethodNameIndex

  object MethodDescriptorIndex

  object MethodAttributes

  def methodInfo(nameIndex: Int, descriptorIndex: Int, attributes: Seq[MetaObject], flags: Set[AccessFlags] = Set()) =
    new MetaObject(MethodInfoKey) {
      data.put(MethodAttributes, attributes)
      data.put(MethodNameIndex, nameIndex)
      data.put(MethodDescriptorIndex, descriptorIndex)
      data.put(AccessFlagsKey, flags)
    }

  def getMethodAccessFlags(method: MetaObject) = method(AccessFlagsKey).asInstanceOf[Set[MethodAccessFlag]]

  def getMethodNameIndex(methodInfo: MetaObject) = methodInfo(MethodNameIndex).asInstanceOf[Int]

  def getMethodDescriptorIndex(methodInfo: MetaObject) = methodInfo(MethodDescriptorIndex).asInstanceOf[Int]
}
