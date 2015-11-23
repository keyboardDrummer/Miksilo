package transformations.bytecode

import core.bigrammar.BiGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node}
import core.particles.{PartialSelf, CompilationState, Contract, ParticleWithGrammar}
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.PrintByteCode._

object ByteCodeMethodInfo extends ParticleWithGrammar with AccessFlags {

  object MethodInfoKey extends Key

  object MethodNameIndex extends Key

  object MethodDescriptorIndex extends Key

  object MethodAttributes extends Key

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
    val methods = grammars.create(MethodsGrammar, "methods:" %> methodInfoGrammar.manyVertical.indent(2))
    val membersGrammar = grammars.find(ByteCodeSkeleton.MembersGrammar)
    membersGrammar.inner = membersGrammar.inner %% methods ^^ parseMap(ClassFileKey, PartialSelf, ClassMethodsKey)
  }

  object AccessFlagGrammar
  object MethodInfoGrammar
  def getMethodInfoGrammar(grammars: GrammarCatalogue): BiGrammar = {
    val attributesGrammar = grammars.find(AttributesGrammar)
    val parseAccessFlag = grammars.create(AccessFlagGrammar, "ACC_PUBLIC" ~> produce(PublicAccess) | "ACC_STATIC" ~> produce(StaticAccess) | "ACC_PRIVATE" ~> produce(PrivateAccess))
    val methodHeader: BiGrammar = Seq[BiGrammar](
      "nameIndex:" ~> integer,
      "descriptorIndex:" ~> integer,
      "flags:" ~> parseAccessFlag.manySeparated(", ").seqToSet).
      reduce((l, r) => (l <~ ",") ~~ r)
    val inner = methodHeader % attributesGrammar
    val methodInfoGrammar: BiGrammar = nodeMap(inner, MethodInfoKey, MethodNameIndex, MethodDescriptorIndex, AccessFlagsKey, MethodAttributes)
    grammars.create(MethodInfoGrammar, methodInfoGrammar)
  }

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton) ++ super.dependencies

  override def description: String = "Adds method members to bytecode."
}
