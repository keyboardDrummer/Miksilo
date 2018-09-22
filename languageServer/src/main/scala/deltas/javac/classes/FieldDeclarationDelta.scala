package deltas.javac.classes

import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.node._
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.objects.Declaration
import core.smarts.scopes.objects.Scope
import deltas.ConstraintSkeleton
import deltas.bytecode.extraConstants.TypeConstant
import deltas.bytecode.types.TypeSkeleton
import deltas.bytecode.{ByteCodeFieldInfo, ByteCodeSkeleton}
import deltas.javac.classes.skeleton.JavaClassSkeleton._
import deltas.javac.classes.skeleton._
import deltas.javac.methods.AccessibilityFieldsDelta

object FieldDeclarationDelta extends DeltaWithGrammar with ClassMemberDelta
  with HasDeclarationDelta
  with HasConstraintsDelta {

  override def description: String = "Enables adding a field declaration without an initializer to a Java class."

  object Shape extends NodeShape
  object Type extends NodeField
  object Name extends NodeField

  implicit class Field[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def name: String = node(Name).asInstanceOf[String]
    def _type: T = node(Type).asInstanceOf[T]
  }

  override def dependencies: Set[Contract] = Set(JavaClassSkeleton, TypeConstant, AccessibilityFieldsDelta)

  def field(_type: Node, name: String) = new Node(Shape, Type -> _type, Name -> name)

  def bind(compilation: Compilation, signature: ClassSignature, javaClass: Node): Unit = {

    val fields = getFields(javaClass)
    for (field <- fields)
      bindField(field)

    def bindField(field: Node) = {

      val name: String = Field(field).name
      val _type = field._type
      signature.newFieldInfo(name, _type)
    }
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

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val memberGrammar = find(JavaClassSkeleton.ClassMemberGrammar)
    val typeGrammar = find(TypeSkeleton.JavaTypeGrammar)

    val fieldGrammar = find(AccessibilityFieldsDelta.VisibilityField) ~ find(AccessibilityFieldsDelta.Static) ~
      typeGrammar.as(Type) ~~ identifier.as(Name) ~< ";" asNode Shape
    memberGrammar.addAlternative(fieldGrammar)
  }

  override def getDeclaration(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Declaration = {
    val field: Field[NodePath] = path
    builder.declare(field.name, parentScope, path.getLocation(Name), Some(TypeSkeleton.getType(compilation, builder, field._type, parentScope)))
  }

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
  }

  override def shape: NodeShape = Shape
}
