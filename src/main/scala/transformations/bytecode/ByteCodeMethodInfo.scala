package transformations.bytecode

import core.bigrammar.BiGrammar
import core.document.BlankLine
import core.particles.grammars.GrammarCatalogue
import core.particles.node._
import core.particles.{Contract, DeltaWithGrammar, Language}
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.constants.Utf8Constant
import transformations.bytecode.coreInstructions.ConstantPoolIndexGrammar
import transformations.bytecode.extraConstants.TypeConstant.TypeConstantWrapper
import transformations.javac.types.MethodType.MethodTypeWrapper

object ByteCodeMethodInfo extends DeltaWithGrammar with AccessFlags {

  object MethodInfoKey extends NodeClass

  object MethodNameIndex extends NodeField

  object MethodDescriptorIndex extends NodeField

  object MethodAttributes extends NodeField

  def methodInfo(nameIndex: Int, descriptorIndex: Int, attributes: Seq[Node], flags: Set[MethodAccessFlag] = Set()) =
    new Node(MethodInfoKey,
      MethodAttributes -> attributes,
      MethodNameIndex -> nameIndex,
      MethodDescriptorIndex -> descriptorIndex,
      AccessFlagsKey -> flags)

  def getMethodAttributes[T <: NodeLike](method: T) = method(MethodAttributes).asInstanceOf[Seq[T]]

    def getMethodAccessFlags(method: Node) = method(AccessFlagsKey).asInstanceOf[Set[MethodAccessFlag]]

  def getMethodNameIndex(methodInfo: Node) = methodInfo(MethodNameIndex).asInstanceOf[Int]

  def getMethodDescriptorIndex(methodInfo: Node) = methodInfo(MethodDescriptorIndex).asInstanceOf[Int]

  implicit class ByteCodeMethodInfoWrapper[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def _type: MethodTypeWrapper[T] = new MethodTypeWrapper[T](typeConstant.value)

    def nameIndex: Int = node(MethodNameIndex).asInstanceOf[Int]
    def nameIndex_=(value: Int): Unit = node(MethodNameIndex) = value

    def typeIndex: Int = node(MethodDescriptorIndex).asInstanceOf[Int]
    def typeIndex_=(value: Int): Unit = node(Int) = value

    def typeConstant: TypeConstantWrapper[T] = node(MethodDescriptorIndex).asInstanceOf[T]
    def typeConstant_=(value: TypeConstantWrapper[T]): Unit = node(MethodDescriptorIndex) = value

    def accessFlags: Set[ByteCodeMethodInfo.MethodAccessFlag] =
      node(ByteCodeMethodInfo.AccessFlagsKey).asInstanceOf[Set[ByteCodeMethodInfo.MethodAccessFlag]]
    def accessFlags_=(value: Node): Unit = node(ByteCodeMethodInfo.AccessFlagsKey) = value

    def attributes: Seq[T] = node(MethodAttributes).asInstanceOf[Seq[T]]
  }

  override def inject(state: Language): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getState(state).getBytes(MethodInfoKey) = methodInfo => getMethodByteCode(methodInfo, state)
    ByteCodeSkeleton.getState(state).constantReferences.put(MethodInfoKey, Map(MethodNameIndex -> Utf8Constant.key,
      MethodDescriptorIndex -> Utf8Constant.key))
  }

  def getMethodByteCode(methodInfo: ByteCodeMethodInfoWrapper[Node], state: Language) = {
    getAccessFlagsByteCode(methodInfo) ++
        shortToBytes(methodInfo.nameIndex) ++
        shortToBytes(methodInfo.typeIndex) ++
      getAttributesByteCode(state, methodInfo.attributes)
    }

  object MethodsGrammar
  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    val methodInfoGrammar: BiGrammar = getMethodInfoGrammar(grammars)
    val methods = grammars.create(MethodsGrammar, methodInfoGrammar.manySeparatedVertical(BlankLine).as(ClassMethodsKey))
    val membersGrammar = grammars.find(ByteCodeSkeleton.MembersGrammar)
    membersGrammar.inner = membersGrammar.inner % methods
  }

  object AccessFlagGrammar
  def getMethodInfoGrammar(grammars: GrammarCatalogue): BiGrammar = {
    val attributesGrammar = grammars.find(AttributesGrammar)
    val parseAccessFlag = grammars.create(AccessFlagGrammar,
        "ACC_PUBLIC" ~> produce(PublicAccess) |
        "ACC_STATIC" ~> produce(StaticAccess) |
        "ACC_PRIVATE" ~> produce(PrivateAccess))

    val methodInfoGrammar: BiGrammar = "Method;" %>
      ("name:" ~~> grammars.find(ConstantPoolIndexGrammar).as(MethodNameIndex) %
      "descriptor:" ~~> grammars.find(ConstantPoolIndexGrammar).as(MethodDescriptorIndex) %
      "flags:" ~~> parseAccessFlag.manySeparated(", ").seqToSet.as(AccessFlagsKey) %
      attributesGrammar.as(MethodAttributes)).indent().asNode(MethodInfoKey)

    grammars.create(MethodInfoKey, methodInfoGrammar)
  }

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton) ++ super.dependencies

  override def description: String = "Adds method members to bytecode."
}
