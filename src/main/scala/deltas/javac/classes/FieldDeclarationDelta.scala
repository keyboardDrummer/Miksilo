package deltas.javac.classes

import core.deltas.grammars.LanguageGrammars
import core.deltas.node._
import core.deltas.path.{ChildPath, Path}
import core.deltas.{Compilation, Contract, DeltaWithGrammar}
import core.language.Language
import core.smarts.ConstraintBuilder
import core.smarts.objects.Declaration
import core.smarts.scopes.objects.Scope
import deltas.bytecode.extraConstants.TypeConstant
import deltas.bytecode.types.TypeSkeleton
import deltas.bytecode.{ByteCodeFieldInfo, ByteCodeSkeleton}
import deltas.javac.classes.skeleton.JavaClassSkeleton._
import deltas.javac.classes.skeleton.{ClassMemberDelta, ClassSignature, HasDeclaration, JavaClassSkeleton}
import deltas.javac.methods.AccessibilityFieldsDelta

object FieldDeclarationDelta extends DeltaWithGrammar with ClassMemberDelta with HasDeclaration {

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
    field.shape = ByteCodeFieldInfo.FieldKey

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
    memberGrammar.addOption(fieldGrammar)
  }

  override def description: String = "Enables adding a field declaration without an initializer to a Java class."


  override def inject(language: Language): Unit = {
    super.inject(language)
    JavaClassSkeleton.hasDeclarations.add(language, Shape, this)
  }

  override def getDeclaration(compilation: Compilation, builder: ConstraintBuilder, path: Path, parentScope: Scope): Declaration = {
    val field: Field[Path] = path
    builder.declare(field.name, path.asInstanceOf[ChildPath], parentScope, Some(TypeSkeleton.getType(compilation, builder, field._type, parentScope)))
  }
}
