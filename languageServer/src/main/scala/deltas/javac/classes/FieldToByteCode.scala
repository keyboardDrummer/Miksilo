package deltas.javac.classes

import core.deltas.Contract
import core.language.node.Node
import core.language.{Compilation, Language}
import deltas.HasNameDelta.Name
import deltas.bytecode.ByteCodeFieldInfo
import deltas.bytecode.extraConstants.TypeConstant
import deltas.javac.classes.FieldDeclarationDelta.{Field, Type}
import deltas.javac.classes.skeleton.JavaClassDelta

object FieldToByteCode {

  def dependencies = Set[Contract](TypeConstant)

  def compile(compilation: Compilation, field: Node): Node = {
    val classCompiler = JavaClassDelta.getClassCompiler(compilation)

    convertField(field, classCompiler, compilation)
    field
  }

  def convertField(node: Node, classCompiler: ClassCompiler, state: Language) {
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
