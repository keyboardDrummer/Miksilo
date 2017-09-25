package transformations.bytecode.extraConstants

import core.bigrammar.BiGrammar
import core.particles.CompilationState
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeClass, NodeField}
import transformations.bytecode.ByteCodeFieldInfo.{DescriptorIndex, NameIndex}
import transformations.bytecode.ByteCodeMethodInfo.{MethodDescriptorIndex, MethodInfoKey, MethodNameIndex}
import transformations.bytecode.constants.MethodTypeConstant.MethodTypeDescriptorIndex
import transformations.bytecode.constants.NameAndTypeConstant.{NameAndTypeName, NameAndTypeType}
import transformations.bytecode.constants.{ConstantEntry, MethodTypeConstant, NameAndTypeConstant, Utf8Constant}
import transformations.bytecode.types.TypeSkeleton
import transformations.bytecode.{ByteCodeFieldInfo, ByteCodeSkeleton, PrintByteCode}

object TypeConstant extends ConstantEntry {
  object Key extends NodeClass
  object Type extends NodeField

  def constructor(_type: Node) = new Node(Key, Type -> _type)
  def getValue(constant: Node) = constant(Type).asInstanceOf[Node]

  override def key = Key

  override def getByteCode(constant: Node, state: CompilationState): Seq[Byte] = {
    val _type: Node = constant(Type).asInstanceOf[Node]
    val typeString = TypeSkeleton.getByteCodeString(state)(_type)
    PrintByteCode.toUTF8ConstantEntry(typeString)
  }

  override def inject(state: CompilationState): Unit = {
    super.inject(state)

    ByteCodeSkeleton.getState(state).constantReferences.put(ByteCodeFieldInfo.FieldKey, Map(
      NameIndex -> Utf8Constant.key,
      DescriptorIndex -> TypeConstant.key))
    ByteCodeSkeleton.getState(state).constantReferences.put(MethodInfoKey, Map(
      MethodNameIndex -> Utf8Constant.key,
      MethodDescriptorIndex -> TypeConstant.key))
    ByteCodeSkeleton.getState(state).constantReferences.put(MethodTypeConstant.key, Map(
      MethodTypeDescriptorIndex -> TypeConstant.key))
    ByteCodeSkeleton.getState(state).constantReferences.put(NameAndTypeConstant.key, Map(
      NameAndTypeName -> Utf8Constant.key,
      NameAndTypeType -> TypeConstant.key))
  }

  override def getConstantEntryGrammar(grammars: GrammarCatalogue): BiGrammar = {
    val typeGrammar = grammars.find(TypeSkeleton.ByteCodeTypeGrammar)
    typeGrammar.as(Type).asNode(key)
  }

  override def description: String = "Adds the field descriptor constant. It contains the type of a field."

  override def getName = "Utf8" //TODO do I want this?
}
