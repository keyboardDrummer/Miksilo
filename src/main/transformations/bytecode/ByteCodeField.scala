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

  def getFieldsByteCode(clazz: MetaObject, state: TransformationState) = {

    val fields = ByteCodeSkeleton.getClassFields(clazz)
    PrintByteCode.shortToBytes(fields.length) ++ fields.map(emitField)

    def emitField(field: MetaObject) : Seq[Byte] = {
      PrintByteCode.getAccessFlagsByteCode(field(AccessFlagsKey).asInstanceOf[MetaObject]) ++
        PrintByteCode.shortToBytes(field(ByteCodeField.FieldNameIndex).asInstanceOf[Int]) ++
        PrintByteCode.shortToBytes(field(ByteCodeField.DescriptorIndex).asInstanceOf[Int]) ++
        ByteCodeAttributes.getAttributesByteCode(clazz, state, field(ByteCodeField.FieldAttributes).asInstanceOf[Seq[MetaObject]])
    }
  }

}
