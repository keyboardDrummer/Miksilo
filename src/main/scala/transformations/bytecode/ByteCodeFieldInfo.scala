package transformations.bytecode

import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node}
import core.particles.{CompilationState, Contract, ParticleWithGrammar}
import transformations.bytecode.ByteCodeSkeleton.{AttributesGrammar, ClassFields, ClassFileKey}

object ByteCodeFieldInfo extends ParticleWithGrammar with AccessFlags {
  object FieldKey extends Key
  object NameIndex extends Key
  object DescriptorIndex extends Key
  object FieldAttributes extends Key

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton)

  def field(nameIndex: Int, descriptorIndex: Int, attributes: Seq[Node]) = {
    new Node(FieldKey, NameIndex -> nameIndex, DescriptorIndex -> descriptorIndex, FieldAttributes -> attributes)
  }

  def emitField(field: Node, state: CompilationState): Seq[Byte] = {
      getAccessFlagsByteCode(field) ++
        PrintByteCode.shortToBytes(field(ByteCodeFieldInfo.NameIndex).asInstanceOf[Int]) ++
        PrintByteCode.shortToBytes(field(ByteCodeFieldInfo.DescriptorIndex).asInstanceOf[Int]) ++
        PrintByteCode.getAttributesByteCode(state, field(ByteCodeFieldInfo.FieldAttributes).asInstanceOf[Seq[Node]])
    }

  override def inject(state: CompilationState): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getState(state).getBytes(FieldKey) = field => emitField(field, state)
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val attributesGrammar = grammars.find(AttributesGrammar)
    val fieldGrammar = nodeMap((("nameIndex:" ~> integer) ~~ (", descriptorIndex" ~> integer) <~ ", attributes") % attributesGrammar,
      FieldKey, NameIndex, DescriptorIndex, FieldAttributes)
    val parseFields = "fields:" %> fieldGrammar.manyVertical.indent()

    val membersGrammar = grammars.find(ByteCodeSkeleton.MembersGrammar)
    membersGrammar.inner = parseFields ~ membersGrammar.inner ^^ parseMap(ClassFileKey, ClassFields, PartialSelf)
  }
  
  override def description: String = "Adds field members to bytecode."
}
