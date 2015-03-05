package transformations.bytecode

import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.bytecode.ByteCodeSkeleton.{ClassFields, ClassFileKey}

object ByteCodeField extends GrammarTransformation with AccessFlags {
  object FieldKey
  object NameIndex
  object DescriptorIndex
  object FieldAttributes

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton)

  def field(nameIndex: Int, descriptorIndex: Int, attributes: Seq[MetaObject]) = {
    new MetaObject(FieldKey, NameIndex -> nameIndex, DescriptorIndex -> descriptorIndex, FieldAttributes -> attributes)
  }

  def emitField(field: MetaObject, state: TransformationState): Seq[Byte] = {
      PrintByteCode.getAccessFlagsByteCode(field(AccessFlagsKey).asInstanceOf[MetaObject]) ++
        PrintByteCode.shortToBytes(field(ByteCodeField.NameIndex).asInstanceOf[Int]) ++
        PrintByteCode.shortToBytes(field(ByteCodeField.DescriptorIndex).asInstanceOf[Int]) ++
        PrintByteCode.getAttributesByteCode(state, field(ByteCodeField.FieldAttributes).asInstanceOf[Seq[MetaObject]])
    }

  override def inject(state: TransformationState): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getState(state).getBytes(FieldKey) = field => emitField(field, state)
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val parseFields = "fields:" %> produce(Seq.empty[Any])

    val membersGrammar = grammars.find(ByteCodeSkeleton.MembersGrammar)
    membersGrammar.inner = parseFields ~ membersGrammar.inner ^^ parseMap(ClassFileKey, ClassFields, PartialSelf)
  }
}
