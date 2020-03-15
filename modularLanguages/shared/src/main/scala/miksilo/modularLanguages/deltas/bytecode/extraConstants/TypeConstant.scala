package miksilo.modularLanguages.deltas.bytecode.extraConstants

import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.node.{Node, NodeField, NodeLike, NodeShape}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.deltas.bytecode.ByteCodeFieldInfo.{DescriptorIndex, NameIndex}
import miksilo.modularLanguages.deltas.bytecode.ByteCodeMethodInfo.{MethodDescriptor, MethodNameIndex, Shape}
import miksilo.modularLanguages.deltas.bytecode.constants.MethodTypeConstant.MethodTypeDescriptorIndex
import miksilo.modularLanguages.deltas.bytecode.constants.{ConstantPoolEntry, MethodTypeConstant, NameAndTypeConstant, Utf8ConstantDelta}
import miksilo.modularLanguages.deltas.bytecode.types.TypeSkeleton
import miksilo.modularLanguages.deltas.bytecode.{ByteCodeFieldInfo, ByteCodeMethodInfo, ByteCodeSkeleton, PrintByteCode}

object TypeConstant extends ConstantPoolEntry {
  object Key extends NodeShape
  object Type extends NodeField

  def constructor(_type: Node) = new Node(Key, Type -> _type)

  implicit class TypeConstantWrapper[T <: NodeLike](node: T) {
    def value: T = node(Type).asInstanceOf[T]
    def value_=(value: T): Unit = node(Type) = value
  }

  override def shape = Key

  override def getBytes(compilation: Compilation, constant: Node): Seq[Byte] = {
    val _type: Node = constant(Type).asInstanceOf[Node]
    val typeString = TypeSkeleton.getByteCodeString(compilation)(_type)
    PrintByteCode.toUTF8ConstantEntry(typeString)
  }

  override def inject(language: Language): Unit = {
    super.inject(language)

    ByteCodeSkeleton.constantReferences.add(language, ByteCodeFieldInfo.Shape, Map(
      NameIndex -> Utf8ConstantDelta.shape,
      DescriptorIndex -> TypeConstant.shape))
    ByteCodeSkeleton.constantReferences.add(language, Shape, Map(
      MethodNameIndex -> Utf8ConstantDelta.shape,
      MethodDescriptor -> TypeConstant.shape))
    ByteCodeSkeleton.constantReferences.add(language, MethodTypeConstant.shape, Map(
      MethodTypeDescriptorIndex -> TypeConstant.shape))
    ByteCodeSkeleton.constantReferences.add(language, NameAndTypeConstant.shape, Map(
      NameAndTypeConstant.Name -> Utf8ConstantDelta.shape,
      NameAndTypeConstant.Type -> TypeConstant.shape))
  }

  override def dependencies = Set(MethodTypeConstant, NameAndTypeConstant, ByteCodeMethodInfo, ByteCodeFieldInfo)

  override def getConstantEntryGrammar(grammars: LanguageGrammars): BiGrammar = {
    val typeGrammar = grammars.find(TypeSkeleton.ByteCodeTypeGrammar)
    import grammars._
    typeGrammar.as(Type)
  }

  override def description: String = "Adds the field descriptor constant. It contains the type of a field."

  override val getName = "Utf8" //TODO do I want this?
}
