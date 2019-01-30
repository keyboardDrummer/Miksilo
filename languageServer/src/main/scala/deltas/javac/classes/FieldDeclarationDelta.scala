package deltas.javac.classes

import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.node._
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.objects.Declaration
import core.smarts.scopes.objects.Scope
import deltas.bytecode.ByteCodeFieldInfo
import deltas.bytecode.extraConstants.TypeConstant
import deltas.bytecode.types.TypeSkeleton
import deltas.javac.classes.skeleton.JavaClassDelta._
import deltas.javac.classes.skeleton._
import deltas.javac.methods.AccessibilityFieldsDelta
import deltas.javac.methods.AccessibilityFieldsDelta.HasAccessibility

object FieldDeclarationDelta extends DeltaWithGrammar
  with HasDeclarationDelta
  with HasConstraintsDelta {

  override def description: String = "Enables adding a field declaration without an initializer to a Java class."

  object Shape extends NodeShape
  object Type extends NodeField
  object Name extends NodeField

  implicit class Field[T <: NodeLike](node: T) extends HasAccessibility[T](node) {
    def name: String = node.getValue(Name).asInstanceOf[String]
    def _type: T = node(Type).asInstanceOf[T]
  }

  override def dependencies: Set[Contract] = Set(TypeConstant, AccessibilityFieldsDelta)

  def neww(_type: Node, name: String) = new Node(Shape, Type -> _type, Name -> name)

  def bind(compilation: Compilation, signature: ClassSignature, field: Node): Unit = {
    val name: String = Field(field).name
    val _type = field._type
    signature.newFieldInfo(name, _type)
  }

  def getFields(javaClass: JavaClass[Node]): Seq[Node] = {
    javaClass.members.filter(member => member.shape == Shape)
  }

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

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val typeGrammar = find(TypeSkeleton.JavaTypeGrammar)

    val fieldGrammar = find(AccessibilityFieldsDelta.VisibilityField) ~ find(AccessibilityFieldsDelta.Static) ~
      typeGrammar.as(Type) ~~ identifier.as(Name) ~< ";" asNode Shape
    create(Shape, fieldGrammar)
  }

  override def getDeclaration(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Declaration = {
    val field: Field[NodePath] = path
    builder.declare(field.name, parentScope, path.getSourceElement(Name), Some(TypeSkeleton.getType(compilation, builder, field._type, parentScope)))
  }

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
  }

  override def shape: NodeShape = Shape
}
