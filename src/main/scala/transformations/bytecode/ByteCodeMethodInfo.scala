package transformations.bytecode

import core.bigrammar.BiGrammar
import core.document.BlankLine
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeClass, NodeField, NodeLike}
import core.particles.{CompilationState, Contract, DeltaWithGrammar}
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.constants.Utf8Constant
import transformations.bytecode.coreInstructions.ConstantPoolIndexGrammar

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

  override def inject(state: CompilationState): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getState(state).getBytes(MethodInfoKey) = methodInfo => getMethodByteCode(methodInfo, state)
    ByteCodeSkeleton.getState(state).constantReferences.put(MethodInfoKey, Map(MethodNameIndex -> Utf8Constant.key,
      MethodDescriptorIndex -> Utf8Constant.key))
  }

  def getMethodByteCode(methodInfo: Node, state: CompilationState) = {
    getAccessFlagsByteCode(methodInfo) ++
        shortToBytes(getMethodNameIndex(methodInfo)) ++
        shortToBytes(getMethodDescriptorIndex(methodInfo)) ++
      getAttributesByteCode(state, ByteCodeMethodInfo.getMethodAttributes(methodInfo))
    }

  object MethodsGrammar
  override def transformGrammars(grammars: GrammarCatalogue, state: CompilationState): Unit = {
    val methodInfoGrammar: BiGrammar = getMethodInfoGrammar(grammars)
    val methods = grammars.create(MethodsGrammar, methodInfoGrammar.manySeparatedVertical(BlankLine).as(ClassMethodsKey))
    val membersGrammar = grammars.find(ByteCodeSkeleton.MembersGrammar)
    membersGrammar.inner = membersGrammar.inner % methods
  }

  object AccessFlagGrammar
  object MethodInfoGrammar
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

    grammars.create(MethodInfoGrammar, methodInfoGrammar)
  }

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton) ++ super.dependencies

  override def description: String = "Adds method members to bytecode."
}
