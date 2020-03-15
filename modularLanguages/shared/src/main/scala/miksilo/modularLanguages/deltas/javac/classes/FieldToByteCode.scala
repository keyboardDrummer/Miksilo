package miksilo.modularLanguages.deltas.javac.classes

import miksilo.modularLanguages.core.deltas.Contract
import miksilo.modularLanguages.core.node.Node
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.deltas.HasNameDelta.Name
import miksilo.modularLanguages.deltas.bytecode.ByteCodeFieldInfo
import miksilo.modularLanguages.deltas.bytecode.extraConstants.TypeConstant
import miksilo.modularLanguages.deltas.javac.classes.FieldDeclarationDelta.{Field, Type}
import miksilo.modularLanguages.deltas.javac.classes.skeleton.JavaClassDelta

object FieldToByteCode {

  def dependencies = Set[Contract](TypeConstant)

  def compile(compilation: Compilation, field: Node): Node = {
    val classCompiler = JavaClassDelta.getClassCompiler(compilation)

    convertField(field, classCompiler, compilation)
    field
  }

  def convertField(node: Node, classCompiler: ClassCompiler, state: Language): Unit = {
    val field: Field[Node] = node
    val nameIndex = classCompiler.getNameIndex(field.name)

    field(ByteCodeFieldInfo.NameIndex) = nameIndex
    field.shape = ByteCodeFieldInfo.Shape

    val fieldDescriptor = TypeConstant.constructor(field._type)
    field(ByteCodeFieldInfo.DescriptorIndex) = fieldDescriptor
    field(ByteCodeFieldInfo.AccessFlagsKey) = Set.empty
    field(ByteCodeFieldInfo.FieldAttributes) = Seq.empty

    field.data.remove(Name)
    field.data.remove(Type)
  }
}
