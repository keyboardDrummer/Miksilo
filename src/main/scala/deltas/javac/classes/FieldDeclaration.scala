package deltas.javac.classes

import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeShape, NodeField}
import core.deltas.{Compilation, Contract, DeltaWithGrammar, Language}
import deltas.bytecode.extraConstants.TypeConstant
import deltas.bytecode.types.TypeSkeleton
import deltas.bytecode.{ByteCodeFieldInfo, ByteCodeSkeleton}
import deltas.javac.classes.skeleton.JavaClassSkeleton._
import deltas.javac.classes.skeleton.{ClassMemberDelta, ClassSignature, JavaClassSkeleton}
import deltas.javac.methods.AccessibilityFieldsDelta

object FieldDeclaration extends DeltaWithGrammar with ClassMemberDelta {

  object Shape extends NodeShape
  object Type extends NodeField
  object Name extends NodeField

  override def dependencies: Set[Contract] = Set(JavaClassSkeleton, TypeConstant, AccessibilityFieldsDelta)

  def field(_type: Node, name: String) = new Node(Shape, Type -> _type, Name -> name)

  def bind(compilation: Compilation, signature: ClassSignature, javaClass: Node): Unit = {

    val fields = getFields(javaClass)
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

  def getFields(javaClass: JavaClass[Node]): Seq[Node] = {
    javaClass.members.filter(member => member.shape == Shape)
  }

  def compile(compilation: Compilation, javaClass: Node): Unit = {
    val classCompiler = JavaClassSkeleton.getClassCompiler(compilation)

    val fields = getFields(javaClass)
    javaClass(ByteCodeSkeleton.ClassFields) = fields.map(field => {
      convertField(field, classCompiler, compilation)
      field
    })
  }

  def convertField(field: Node, classCompiler: ClassCompiler, state: Language) {
    val nameIndex = classCompiler.getNameIndex(getFieldName(field))

    field(ByteCodeFieldInfo.NameIndex) = nameIndex
    field.shape = ByteCodeFieldInfo.FieldKey

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
      typeGrammar.as(Type) ~~ identifier.as(Name) ~< ";" asNode Shape
    memberGrammar.addOption(fieldGrammar)
  }

  override def description: String = "Enables adding a field declaration without an initializer to a Java class."
}
