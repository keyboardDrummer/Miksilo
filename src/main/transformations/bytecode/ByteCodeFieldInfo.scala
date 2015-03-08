package transformations.bytecode

import core.transformation.grammars.GrammarCatalogue
import core.transformation.{ParticleWithGrammar, Contract, MetaObject, CompilationState}
import transformations.bytecode.ByteCodeSkeleton.{AttributesGrammar, ClassFields, ClassFileKey}

object ByteCodeFieldInfo extends ParticleWithGrammar with AccessFlags {
  object FieldKey
  object NameIndex
  object DescriptorIndex
  object FieldAttributes

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton)

  def field(nameIndex: Int, descriptorIndex: Int, attributes: Seq[MetaObject]) = {
    new MetaObject(FieldKey, NameIndex -> nameIndex, DescriptorIndex -> descriptorIndex, FieldAttributes -> attributes)
  }

  def emitField(field: MetaObject, state: CompilationState): Seq[Byte] = {
      getAccessFlagsByteCode(field) ++
        PrintByteCode.shortToBytes(field(ByteCodeFieldInfo.NameIndex).asInstanceOf[Int]) ++
        PrintByteCode.shortToBytes(field(ByteCodeFieldInfo.DescriptorIndex).asInstanceOf[Int]) ++
        PrintByteCode.getAttributesByteCode(state, field(ByteCodeFieldInfo.FieldAttributes).asInstanceOf[Seq[MetaObject]])
    }

  override def inject(state: CompilationState): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getState(state).getBytes(FieldKey) = field => emitField(field, state)
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val attributesGrammar = grammars.find(AttributesGrammar)
    val fieldGrammar = "nameIndex:" ~~ integer ~~ ", descriptorIndex" ~~ integer ~~ ", attributes" % attributesGrammar ^^
      parseMap(FieldKey, NameIndex, DescriptorIndex, FieldAttributes)
    val parseFields = "fields:" %> fieldGrammar.manyVertical.indent()

    val membersGrammar = grammars.find(ByteCodeSkeleton.MembersGrammar)
    membersGrammar.inner = parseFields ~ membersGrammar.inner ^^ parseMap(ClassFileKey, ClassFields, PartialSelf)
  }
  
  override def description: String = "Adds field members to bytecode."
}
