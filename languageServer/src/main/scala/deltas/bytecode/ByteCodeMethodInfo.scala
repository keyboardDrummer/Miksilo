package deltas.bytecode

import core.bigrammar.BiGrammar
import core.document.BlankLine
import core.deltas.grammars.LanguageGrammars
import core.language.node._
import core.deltas.{Contract, DeltaWithGrammar, HasShape}
import core.language.{Compilation, Language}
import deltas.bytecode.ByteCodeSkeleton._
import deltas.bytecode.PrintByteCode._
import deltas.bytecode.attributes.CodeAttributeDelta
import deltas.bytecode.attributes.CodeAttributeDelta.CodeAttribute
import deltas.bytecode.constants.Utf8ConstantDelta
import deltas.bytecode.coreInstructions.ConstantPoolIndexGrammar
import deltas.bytecode.extraConstants.TypeConstant.TypeConstantWrapper
import deltas.javac.types.MethodTypeDelta.MethodType

object ByteCodeMethodInfo extends DeltaWithGrammar with AccessFlags with HasBytes with HasShape {

  object Shape extends NodeShape

  object MethodNameIndex extends NodeField

  object MethodDescriptor extends NodeField

  object MethodAttributes extends NodeField

  def methodInfo(nameIndex: Int, descriptorIndex: Int, attributes: Seq[Node], flags: Set[MethodAccessFlag] = Set()) =
    new Node(Shape,
      MethodAttributes -> attributes,
      MethodNameIndex -> nameIndex,
      MethodDescriptor -> descriptorIndex,
      AccessFlagsKey -> flags)

  implicit class MethodInfo[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def _type: MethodType[T] = new MethodType[T](typeConstant.value)

    def nameIndex: Int = node(MethodNameIndex).asInstanceOf[Int]
    def nameIndex_=(value: Int): Unit = node(MethodNameIndex) = value

    def typeIndex: Int = node(MethodDescriptor).asInstanceOf[Int]
    def typeIndex_=(value: Int): Unit = node(MethodDescriptor) = value

    def typeConstant: TypeConstantWrapper[T] = node(MethodDescriptor).asInstanceOf[T]
    def typeConstant_=(value: TypeConstantWrapper[T]): Unit = node(MethodDescriptor) = value

    def accessFlags: Set[ByteCodeMethodInfo.MethodAccessFlag] =
      node(ByteCodeMethodInfo.AccessFlagsKey).asInstanceOf[Set[ByteCodeMethodInfo.MethodAccessFlag]]
    def accessFlags_=(value: Node): Unit = node(ByteCodeMethodInfo.AccessFlagsKey) = value

    def attributes: Seq[T] = node(MethodAttributes).asInstanceOf[Seq[T]]

    def codeAttribute: CodeAttribute[T] = attributes.find(r => r.shape == CodeAttributeDelta.shape).get
  }

  override def inject(language: Language): Unit = {
    super.inject(language)
    ByteCodeSkeleton.hasBytes.add(language, this)
    ByteCodeSkeleton.constantReferences.add(language, Shape, Map(MethodNameIndex -> Utf8ConstantDelta.shape,
      MethodDescriptor -> Utf8ConstantDelta.shape))
  }

  def getBytes(compilation: Compilation, node: Node): Seq[Byte] = {
    val methodInfo: MethodInfo[Node] = node
    getAccessFlagsByteCode(methodInfo) ++
        shortToBytes(methodInfo.nameIndex) ++
        shortToBytes(methodInfo.typeIndex) ++
      getAttributesByteCode(compilation, methodInfo.attributes)
    }

  object MethodsGrammar extends GrammarKey
  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    val methodInfoGrammar: BiGrammar = getMethodInfoGrammar(grammars)
    import grammars._
    val methods = create(MethodsGrammar, methodInfoGrammar.manySeparatedVertical(BlankLine).as(Methods))
    val membersGrammar = find(ByteCodeSkeleton.MembersGrammar)
    membersGrammar.inner = membersGrammar.inner %> methods
  }

  object AccessFlagGrammar extends GrammarKey
  def getMethodInfoGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    val attributesGrammar = find(AttributesGrammar)
    val parseAccessFlag = create(AccessFlagGrammar,
        "ACC_PUBLIC" ~> valueGrammar(PublicAccess) |
        "ACC_STATIC" ~> valueGrammar(StaticAccess) |
        "ACC_PRIVATE" ~> valueGrammar(PrivateAccess))

    val methodInfoGrammar: BiGrammar = "Method" ~ ";"  %>
      ("name" ~ ":" ~~> find(ConstantPoolIndexGrammar).as(MethodNameIndex) %
      "descriptor" ~ ":" ~~> find(ConstantPoolIndexGrammar).as(MethodDescriptor) %
      "flags" ~ ":" ~~> parseAccessFlag.manySeparated("," ~ printSpace).seqToSet.as(AccessFlagsKey) %
      attributesGrammar.as(MethodAttributes)).indent().asNode(Shape)

    create(Shape, methodInfoGrammar)
  }

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton)

  override def description: String = "Adds method members to bytecode."

  override def shape: NodeShape = Shape
}
