package transformations.bytecode.extraConstants

import core.bigrammar.BiGrammar
import core.particles.Language
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeClass, NodeField, NodeLike}
import transformations.bytecode.ByteCodeFieldInfo.{DescriptorIndex, NameIndex}
import transformations.bytecode.ByteCodeMethodInfo.{MethodDescriptor, MethodInfoKey, MethodNameIndex}
import transformations.bytecode.constants.MethodTypeConstant.MethodTypeDescriptorIndex
import transformations.bytecode.constants.{ConstantEntry, MethodTypeConstant, NameAndTypeConstant, Utf8ConstantDelta}
import transformations.bytecode.types.TypeSkeleton
import transformations.bytecode.{ByteCodeFieldInfo, ByteCodeMethodInfo, ByteCodeSkeleton, PrintByteCode}

object TypeConstant extends ConstantEntry {
  object Key extends NodeClass
  object Type extends NodeField

  def constructor(_type: Node) = new Node(Key, Type -> _type)

  implicit class TypeConstantWrapper[T <: NodeLike](node: T) {
    def value: T = node(Type).asInstanceOf[T]
    def value_=(value: T): Unit = node(Type) = value
  }

  override def key = Key

  override def getByteCode(constant: Node, state: Language): Seq[Byte] = {
    val _type: Node = constant(Type).asInstanceOf[Node]
    val typeString = TypeSkeleton.getByteCodeString(state)(_type)
    PrintByteCode.toUTF8ConstantEntry(typeString)
  }

  override def inject(state: Language): Unit = {
    super.inject(state)

    ByteCodeSkeleton.getRegistry(state).constantReferences.put(ByteCodeFieldInfo.FieldKey, Map(
      NameIndex -> Utf8ConstantDelta.key,
      DescriptorIndex -> TypeConstant.key))
    ByteCodeSkeleton.getRegistry(state).constantReferences.put(MethodInfoKey, Map(
      MethodNameIndex -> Utf8ConstantDelta.key,
      MethodDescriptor -> TypeConstant.key))
    ByteCodeSkeleton.getRegistry(state).constantReferences.put(MethodTypeConstant.key, Map(
      MethodTypeDescriptorIndex -> TypeConstant.key))
    ByteCodeSkeleton.getRegistry(state).constantReferences.put(NameAndTypeConstant.key, Map(
      NameAndTypeConstant.Name -> Utf8ConstantDelta.key,
      NameAndTypeConstant.Type -> TypeConstant.key))
  }

  override def dependencies = Set(MethodTypeConstant, NameAndTypeConstant, ByteCodeMethodInfo, ByteCodeFieldInfo)

  override def getConstantEntryGrammar(grammars: GrammarCatalogue): BiGrammar = {
    val typeGrammar = grammars.find(TypeSkeleton.ByteCodeTypeGrammar)
    typeGrammar.as(Type)
  }

  override def description: String = "Adds the field descriptor constant. It contains the type of a field."

  override def getName = "Utf8" //TODO do I want this?
}
