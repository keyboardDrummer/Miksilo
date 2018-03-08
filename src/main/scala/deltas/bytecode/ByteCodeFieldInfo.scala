package deltas.bytecode

import core.deltas.grammars.LanguageGrammars
import core.deltas.{Contract, DeltaWithGrammar, HasShape}
import core.document.BlankLine
import core.language.node._
import core.language.{Compilation, Language}
import deltas.bytecode.ByteCodeSkeleton.{AttributesGrammar, ClassFields, HasBytes}
import deltas.bytecode.constants.Utf8ConstantDelta
import deltas.bytecode.coreInstructions.ConstantPoolIndexGrammar

object ByteCodeFieldInfo extends DeltaWithGrammar with AccessFlags with HasBytes with HasShape {
  object Shape extends NodeShape
  object NameIndex extends NodeField
  object DescriptorIndex extends NodeField
  object FieldAttributes extends NodeField

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton)

  def field(nameIndex: Int, descriptorIndex: Int, attributes: Seq[Node]) = {
    new Node(Shape, NameIndex -> nameIndex, DescriptorIndex -> descriptorIndex, FieldAttributes -> attributes)
  }

  implicit class FieldInfoWrapper[T <: NodeLike](val node: T) extends NodeWrapper[T] {

  }

  def getBytes(compilation: Compilation, field: Node): Seq[Byte] = {
      getAccessFlagsByteCode(field) ++
        PrintByteCode.shortToBytes(field(ByteCodeFieldInfo.NameIndex).asInstanceOf[Int]) ++
        PrintByteCode.shortToBytes(field(ByteCodeFieldInfo.DescriptorIndex).asInstanceOf[Int]) ++
        PrintByteCode.getAttributesByteCode(compilation, field(ByteCodeFieldInfo.FieldAttributes).asInstanceOf[Seq[Node]])
    }

  override def inject(language: Language): Unit = {
    super.inject(language)
    ByteCodeSkeleton.hasBytes.add(language, this)
    ByteCodeSkeleton.constantReferences.add(language, Shape, Map(
      NameIndex -> Utf8ConstantDelta.shape,
      DescriptorIndex -> Utf8ConstantDelta.shape))
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._

    val attributesGrammar = find(AttributesGrammar)
    val constantIndex = find(ConstantPoolIndexGrammar)
    val fieldGrammar = "Field" ~ ";" %>
      ("name" ~ ":" ~~> constantIndex.as(NameIndex) %
        ("descriptor" ~ ":" ~~> constantIndex.as(DescriptorIndex)) %
        attributesGrammar.as(FieldAttributes)).asLabelledNode(Shape).indent()
    val parseFields = (fieldGrammar %< BlankLine).manyVertical.as(ClassFields)

    val membersGrammar = find(ByteCodeSkeleton.MembersGrammar)
    membersGrammar.inner = parseFields ~ membersGrammar.inner
  }
  
  override def description: String = "Adds field members to bytecode."

  override def shape: NodeShape = Shape
}
