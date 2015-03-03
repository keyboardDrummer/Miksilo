package transformations.bytecode

object ByteCodeField extends AccessFlags {
  object FieldKey
  object FieldNameIndex
  object DescriptorIndex
  object FieldAttributes

  def field(nameIndex: Int, descriptorIndex: Int, attributes: Seq[MetaObject]) = {
    new MetaObject(FieldKey, FieldNameIndex -> nameIndex, DescriptorIndex -> descriptorIndex, FieldAttributes -> attributes)
  }
}
