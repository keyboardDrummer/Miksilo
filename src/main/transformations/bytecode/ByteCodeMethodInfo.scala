package transformations.bytecode

import core.transformation.grammars.GrammarCatalogue
import core.transformation.{TransformationState, MetaObject}
import core.transformation.sillyCodePieces.GrammarTransformation
import PrintByteCode._

object ByteCodeMethodInfo extends GrammarTransformation with AccessFlags {

  object MethodInfoKey

  object MethodNameIndex

  object MethodDescriptorIndex

  object MethodAttributes

  def methodInfo(nameIndex: Int, descriptorIndex: Int, attributes: Seq[MetaObject], flags: Set[MethodAccessFlag] = Set()) =
    new MetaObject(MethodInfoKey) {
      data.put(MethodAttributes, attributes)
      data.put(MethodNameIndex, nameIndex)
      data.put(MethodDescriptorIndex, descriptorIndex)
      data.put(AccessFlagsKey, flags)
    }

  def getMethodAccessFlags(method: MetaObject) = method(AccessFlagsKey).asInstanceOf[Set[MethodAccessFlag]]

  def getMethodNameIndex(methodInfo: MetaObject) = methodInfo(MethodNameIndex).asInstanceOf[Int]

  def getMethodDescriptorIndex(methodInfo: MetaObject) = methodInfo(MethodDescriptorIndex).asInstanceOf[Int]


  def getMethodsByteCode(clazz: MetaObject, state: TransformationState): Seq[Byte] = {

    def getMethodByteCode(methodInfo: MetaObject) = {
      val accessCodes = Map(
        PublicAccess -> "0001",
        StaticAccess -> "0008",
        PrivateAccess -> "0002").mapValues(s => hexToInt(s))
      shortToBytes(getMethodAccessFlags(methodInfo).map(flag => accessCodes(flag)).sum) ++
        shortToBytes(getMethodNameIndex(methodInfo)) ++
        shortToBytes(getMethodDescriptorIndex(methodInfo)) ++
        getAttributesByteCode(state, ByteCodeSkeleton.getMethodAttributes(methodInfo))
    }

    val methods = ByteCodeSkeleton.getMethods(clazz)
    shortToBytes(methods.length) ++ methods.flatMap(method => getMethodByteCode(method))
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {

  }
}
