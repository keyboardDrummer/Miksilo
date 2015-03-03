package transformations.bytecode

import core.transformation.{MetaObject, TransformationState}

object ByteCodeField extends AccessFlags {
  object FieldKey
  object FieldNameIndex
  object DescriptorIndex
  object FieldAttributes

  def field(nameIndex: Int, descriptorIndex: Int, attributes: Seq[MetaObject]) = {
    new MetaObject(FieldKey, FieldNameIndex -> nameIndex, DescriptorIndex -> descriptorIndex, FieldAttributes -> attributes)
  }

  def getFieldsByteCode(clazz: MetaObject, state: TransformationState): Seq[Byte] = {

    def emitField(field: MetaObject): Seq[Byte] = {
      PrintByteCode.getAccessFlagsByteCode(field(AccessFlagsKey).asInstanceOf[MetaObject]) ++
        PrintByteCode.shortToBytes(field(ByteCodeField.FieldNameIndex).asInstanceOf[Int]) ++
        PrintByteCode.shortToBytes(field(ByteCodeField.DescriptorIndex).asInstanceOf[Int]) ++
        PrintByteCode.getAttributesByteCode(state, field(ByteCodeField.FieldAttributes).asInstanceOf[Seq[MetaObject]])
    }

    val fields = ByteCodeSkeleton.getClassFields(clazz)
    PrintByteCode.shortToBytes(fields.length) ++ fields.flatMap(emitField)
  }

}
