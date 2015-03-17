package transformations.bytecode

import core.biGrammar.BiGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.{CompilationState, Contract, MetaObject, ParticleWithGrammar}
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.PrintByteCode._

object ByteCodeMethodInfo extends ParticleWithGrammar with AccessFlags {

  object MethodInfoKey

  object MethodNameIndex

  object MethodDescriptorIndex

  object MethodAttributes

  def methodInfo(nameIndex: Int, descriptorIndex: Int, attributes: Seq[MetaObject], flags: Set[MethodAccessFlag] = Set()) =
    new MetaObject(MethodInfoKey,
      MethodAttributes -> attributes,
      MethodNameIndex -> nameIndex,
      MethodDescriptorIndex -> descriptorIndex,
      AccessFlagsKey -> flags)

  def getMethodAttributes(method: MetaObject) = method(MethodAttributes).asInstanceOf[Seq[MetaObject]]

  def getMethodAccessFlags(method: MetaObject) = method(AccessFlagsKey).asInstanceOf[Set[MethodAccessFlag]]

  def getMethodNameIndex(methodInfo: MetaObject) = methodInfo(MethodNameIndex).asInstanceOf[Int]

  def getMethodDescriptorIndex(methodInfo: MetaObject) = methodInfo(MethodDescriptorIndex).asInstanceOf[Int]

  override def inject(state: CompilationState): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getState(state).getBytes(MethodInfoKey) = methodInfo => getMethodByteCode(methodInfo, state)
  }

  def getMethodByteCode(methodInfo: MetaObject, state: CompilationState) = {
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
    val methodInfoGrammar: BiGrammar = methodHeader % attributesGrammar ^^
      parseMap(MethodInfoKey, MethodNameIndex, MethodDescriptorIndex, AccessFlagsKey, MethodAttributes)
    grammars.create(MethodInfoGrammar, methodInfoGrammar)
  }

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton) ++ super.dependencies

  override def description: String = "Adds method members to bytecode."
}
