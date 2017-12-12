package deltas.javac.classes

import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeClass, NodeField}
import core.deltas.{Compilation, Contract, DeltaWithGrammar, Language}
import deltas.bytecode.extraConstants.TypeConstant
import deltas.bytecode.types.TypeSkeleton
import deltas.bytecode.{ByteCodeFieldInfo, ByteCodeSkeleton}
import deltas.javac.classes.skeleton.JavaClassSkeleton._
import deltas.javac.classes.skeleton.{ClassMemberDelta, ClassSignature, JavaClassSkeleton}
import deltas.javac.methods.AccessibilityFieldsDelta

object FieldDeclaration extends DeltaWithGrammar with ClassMemberDelta {

  object Clazz extends NodeClass
  object Type extends NodeField
  object Name extends NodeField

  override def dependencies: Set[Contract] = Set(JavaClassSkeleton, TypeConstant, AccessibilityFieldsDelta)

  def field(_type: Node, name: String) = new Node(Clazz, Type -> _type, Name -> name)

  def bind(compilation: Compilation, signature: ClassSignature, clazz: Node): Unit = {

    val fields = getFields(clazz)
    for (field <- fields)
      bindField(field)

    def bindField(field: Node) = {
      val name: String = getFieldName(field)
      val _type = getFieldType(field)
      signature.newFieldInfo(name, _type)
    }
  }

  def getFieldType(field: Node): Node = {
    field(Type).asInstanceOf[Node]
  }

  def getFieldName(field: Node): String = {
    field(Name).asInstanceOf[String]
  }

  def getFields(clazz: Node): Seq[Node] = {
    clazz.members.filter(member => member.clazz == Clazz)
  }

  def compile(compilation: Compilation, clazz: Node): Unit = {
    val classCompiler = JavaClassSkeleton.getClassCompiler(compilation)

    val fields = getFields(clazz)
    clazz(ByteCodeSkeleton.ClassFields) = fields.map(field => {
      convertField(field, classCompiler, compilation)
      field
    })
  }

  def convertField(field: Node, classCompiler: ClassCompiler, state: Language) {
    val nameIndex = classCompiler.getNameIndex(getFieldName(field))

    field(ByteCodeFieldInfo.NameIndex) = nameIndex
    field.clazz = ByteCodeFieldInfo.FieldKey

    val fieldDescriptor = TypeConstant.constructor(getFieldType(field))
    field(ByteCodeFieldInfo.DescriptorIndex) = fieldDescriptor
    field(ByteCodeFieldInfo.AccessFlagsKey) = Set.empty
    field(ByteCodeFieldInfo.FieldAttributes) = Seq.empty

    field.data.remove(Name)
    field.data.remove(Type)
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val memberGrammar = find(JavaClassSkeleton.ClassMemberGrammar)
    val typeGrammar = find(TypeSkeleton.JavaTypeGrammar)

    val fieldGrammar = find(AccessibilityFieldsDelta.VisibilityField) ~ find(AccessibilityFieldsDelta.Static) ~
      typeGrammar.as(Type) ~~ identifier.as(Name) ~< ";" asNode Clazz
    memberGrammar.addOption(fieldGrammar)
  }

  override def description: String = "Enables adding a field declaration without an initializer to a Java class."
}
