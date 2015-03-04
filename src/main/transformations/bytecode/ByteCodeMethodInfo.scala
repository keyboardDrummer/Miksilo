package transformations.bytecode

import core.grammarDocument.{BiGrammar, ManyVertical}
import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation
import core.transformation.{MetaObject, TransformationState}
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.PrintByteCode._

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

  def getMethodAttributes(method: MetaObject) = method(MethodAttributes).asInstanceOf[Seq[MetaObject]]

  def getMethodAccessFlags(method: MetaObject) = method(AccessFlagsKey).asInstanceOf[Set[MethodAccessFlag]]

  def getMethodNameIndex(methodInfo: MetaObject) = methodInfo(MethodNameIndex).asInstanceOf[Int]

  def getMethodDescriptorIndex(methodInfo: MetaObject) = methodInfo(MethodDescriptorIndex).asInstanceOf[Int]

  override def inject(state: TransformationState): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getState(state).getBytes(MethodInfoKey) = methodInfo => getMethodByteCode(methodInfo, state)
  }

  def getMethodByteCode(methodInfo: MetaObject, state: TransformationState) = {
      val accessCodes = Map(
        PublicAccess -> "0001",
        StaticAccess -> "0008",
        PrivateAccess -> "0002").mapValues(s => hexToInt(s))
      shortToBytes(getMethodAccessFlags(methodInfo).map(flag => accessCodes(flag)).sum) ++
        shortToBytes(getMethodNameIndex(methodInfo)) ++
        shortToBytes(getMethodDescriptorIndex(methodInfo)) ++
      getAttributesByteCode(state, ByteCodeMethodInfo.getMethodAttributes(methodInfo))
    }

  object MethodsGrammar
  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val methodInfoGrammar: BiGrammar = getMethodInfoGrammar(grammars)
    val methods = grammars.create(MethodsGrammar, "methods:" %> methodInfoGrammar.manyVertical.indent(2))
    val membersGrammar = grammars.find(ByteCodeSkeleton.MembersGrammar)
    membersGrammar.inner = membersGrammar.inner %% methods ^^ parseMap(ClassFileKey, ClassFields, ClassMethodsKey) //TODO: remove ClassFields reference.
  }

  object AccessFlagGrammar
  object MethodInfoGrammar
  def getMethodInfoGrammar(grammars: GrammarCatalogue): BiGrammar = {
    val parseAttribute = grammars.find(AttributeGrammar)
    val parseAccessFlag = grammars.create(AccessFlagGrammar, "ACC_PUBLIC" ~> produce(PublicAccess) | "ACC_STATIC" ~> produce(StaticAccess) | "ACC_PRIVATE" ~> produce(PrivateAccess))
    val methodHeader: BiGrammar = Seq[BiGrammar](
      "nameIndex:" ~> integer,
      "descriptorIndex:" ~> integer,
      "flags:" ~> parseAccessFlag.manySeparated(", ").seqToSet).
      reduce((l, r) => (l <~ ",") ~~ r)
    val methodInfoGrammar: BiGrammar = methodHeader % ("attributes:" %> new ManyVertical(parseAttribute).indent(2)) ^^
      parseMap(MethodInfoKey, MethodNameIndex, MethodDescriptorIndex, AccessFlagsKey, MethodAttributes)
    grammars.create(MethodInfoGrammar, methodInfoGrammar)
  }
}
