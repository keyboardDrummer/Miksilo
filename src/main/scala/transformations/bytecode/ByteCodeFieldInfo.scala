package transformations.bytecode

import core.document.BlankLine
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeClass, NodeField}
import core.particles.{Language, Contract, DeltaWithGrammar}
import transformations.bytecode.ByteCodeSkeleton.{AttributesGrammar, ClassFields}
import transformations.bytecode.constants.Utf8Constant
import transformations.bytecode.coreInstructions.ConstantPoolIndexGrammar

object ByteCodeFieldInfo extends DeltaWithGrammar with AccessFlags {
  object FieldKey extends NodeClass
  object NameIndex extends NodeField
  object DescriptorIndex extends NodeField
  object FieldAttributes extends NodeField

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton)

  def field(nameIndex: Int, descriptorIndex: Int, attributes: Seq[Node]) = {
    new Node(FieldKey, NameIndex -> nameIndex, DescriptorIndex -> descriptorIndex, FieldAttributes -> attributes)
  }

  def emitField(field: Node, state: Language): Seq[Byte] = {
      getAccessFlagsByteCode(field) ++
        PrintByteCode.shortToBytes(field(ByteCodeFieldInfo.NameIndex).asInstanceOf[Int]) ++
        PrintByteCode.shortToBytes(field(ByteCodeFieldInfo.DescriptorIndex).asInstanceOf[Int]) ++
        PrintByteCode.getAttributesByteCode(state, field(ByteCodeFieldInfo.FieldAttributes).asInstanceOf[Seq[Node]])
    }

  override def inject(state: Language): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getState(state).getBytes(FieldKey) = field => emitField(field, state)
    ByteCodeSkeleton.getState(state).constantReferences.put(FieldKey, Map(
      NameIndex -> Utf8Constant.key,
      DescriptorIndex -> Utf8Constant.key))
  }

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    val attributesGrammar = grammars.find(AttributesGrammar)
    val constantIndex = grammars.find(ConstantPoolIndexGrammar)
    val fieldGrammar = "Field" ~> ("name:" ~> constantIndex.as(NameIndex) %
      ("descriptor" ~> constantIndex.as(DescriptorIndex)) %
      attributesGrammar.as(FieldAttributes)).asLabelledNode(grammars, FieldKey)
    val parseFields = (fieldGrammar <~ BlankLine).manyVertical.as(ClassFields)

    val membersGrammar = grammars.find(ByteCodeSkeleton.MembersGrammar)
    membersGrammar.inner = parseFields ~ membersGrammar.inner
  }
  
  override def description: String = "Adds field members to bytecode."
}
