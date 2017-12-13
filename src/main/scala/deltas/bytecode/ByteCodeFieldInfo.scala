package deltas.bytecode

import core.document.BlankLine
import core.deltas.grammars.LanguageGrammars
import core.deltas.node._
import core.deltas.{Contract, DeltaWithGrammar, Language}
import deltas.bytecode.ByteCodeSkeleton.{AttributesGrammar, ClassFields}
import deltas.bytecode.constants.Utf8ConstantDelta
import deltas.bytecode.coreInstructions.ConstantPoolIndexGrammar

object ByteCodeFieldInfo extends DeltaWithGrammar with AccessFlags {
  object FieldKey extends NodeShape
  object NameIndex extends NodeField
  object DescriptorIndex extends NodeField
  object FieldAttributes extends NodeField

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton)

  def field(nameIndex: Int, descriptorIndex: Int, attributes: Seq[Node]) = {
    new Node(FieldKey, NameIndex -> nameIndex, DescriptorIndex -> descriptorIndex, FieldAttributes -> attributes)
  }

  implicit class FieldInfoWrapper[T <: NodeLike](val node: T) extends NodeWrapper[T] {

  }

  def emitField(field: Node, state: Language): Seq[Byte] = {
      getAccessFlagsByteCode(field) ++
        PrintByteCode.shortToBytes(field(ByteCodeFieldInfo.NameIndex).asInstanceOf[Int]) ++
        PrintByteCode.shortToBytes(field(ByteCodeFieldInfo.DescriptorIndex).asInstanceOf[Int]) ++
        PrintByteCode.getAttributesByteCode(state, field(ByteCodeFieldInfo.FieldAttributes).asInstanceOf[Seq[Node]])
    }

  override def inject(state: Language): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getRegistry(state).getBytes(FieldKey) = field => emitField(field, state)
    ByteCodeSkeleton.getRegistry(state).constantReferences.put(FieldKey, Map(
      NameIndex -> Utf8ConstantDelta.key,
      DescriptorIndex -> Utf8ConstantDelta.key))
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._

    val attributesGrammar = find(AttributesGrammar)
    val constantIndex = find(ConstantPoolIndexGrammar)
    val fieldGrammar = "Field" ~> ("name:" ~> constantIndex.as(NameIndex) %
      ("descriptor" ~> constantIndex.as(DescriptorIndex)) %
      attributesGrammar.as(FieldAttributes)).asLabelledNode(FieldKey)
    val parseFields = (fieldGrammar ~< BlankLine).manyVertical.as(ClassFields)

    val membersGrammar = find(ByteCodeSkeleton.MembersGrammar)
    membersGrammar.inner = parseFields ~ membersGrammar.inner
  }
  
  override def description: String = "Adds field members to bytecode."
}
