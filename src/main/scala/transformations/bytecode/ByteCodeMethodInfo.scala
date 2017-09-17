package transformations.bytecode

import core.bigrammar.{BiGrammar, MapGrammar}
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node, NodeClass, NodeField}
import core.particles.{CompilationState, Contract, DeltaWithGrammar, FromMap}
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.constants.{MethodTypeConstant, Utf8Constant}
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

  def getMethodAttributes(method: Node) = method(MethodAttributes).asInstanceOf[Seq[Node]]

  def getMethodAccessFlags(method: Node) = method(AccessFlagsKey).asInstanceOf[Set[MethodAccessFlag]]

  def getMethodNameIndex(methodInfo: Node) = methodInfo(MethodNameIndex).asInstanceOf[Int]

  def getMethodDescriptorIndex(methodInfo: Node) = methodInfo(MethodDescriptorIndex).asInstanceOf[Int]

  override def inject(state: CompilationState): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getState(state).getBytes(MethodInfoKey) = methodInfo => getMethodByteCode(methodInfo, state)
    ByteCodeSkeleton.getState(state).constantReferences.put(MethodInfoKey, Map(MethodNameIndex -> Utf8Constant.key, MethodDescriptorIndex -> MethodTypeConstant.key))
  }

  def getMethodByteCode(methodInfo: Node, state: CompilationState) = {
    getAccessFlagsByteCode(methodInfo) ++
        shortToBytes(getMethodNameIndex(methodInfo)) ++
        shortToBytes(getMethodDescriptorIndex(methodInfo)) ++
      getAttributesByteCode(state, ByteCodeMethodInfo.getMethodAttributes(methodInfo))
    }

  object MethodsGrammar
  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val methodInfoGrammar: BiGrammar = getMethodInfoGrammar(grammars)
    val methods = grammars.create(MethodsGrammar, "methods:" %> methodInfoGrammar.manyVertical.indent(2).as(ClassMethodsKey))
    val membersGrammar = grammars.find(ByteCodeSkeleton.MembersGrammar)
    membersGrammar.inner = membersGrammar.inner %% methods
  }

  object AccessFlagGrammar
  object MethodInfoGrammar
  def getMethodInfoGrammar(grammars: GrammarCatalogue): BiGrammar = {
    val attributesGrammar = grammars.find(AttributesGrammar)
    val parseAccessFlag = grammars.create(AccessFlagGrammar, "ACC_PUBLIC" ~> produce(PublicAccess) | "ACC_STATIC" ~> produce(StaticAccess) | "ACC_PRIVATE" ~> produce(PrivateAccess))
    val methodHeader: BiGrammar = Seq[BiGrammar]("method =>" ~~
      "nameIndex:" ~> grammars.find(ConstantPoolIndexGrammar).as(MethodNameIndex),
      "descriptorIndex:" ~> grammars.find(ConstantPoolIndexGrammar).as(MethodDescriptorIndex),
      "flags:" ~> parseAccessFlag.manySeparated(", ").seqToSet.as(AccessFlagsKey)).
      reduce((l, r) => (l <~ ",") ~~ r)
    val inner = methodHeader % attributesGrammar.as(MethodAttributes)
    val methodInfoGrammar: BiGrammar = inner.asNode(MethodInfoKey)
    grammars.create(MethodInfoGrammar, methodInfoGrammar)
  }

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton) ++ super.dependencies

  override def description: String = "Adds method members to bytecode."
}
