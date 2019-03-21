package deltas.javac.classes

import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.node._
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.objects.Declaration
import core.smarts.scopes.objects.Scope
import deltas.HasNameDelta.HasName
import deltas.bytecode.types.TypeSkeleton
import deltas.javac.classes.skeleton.JavaClassDelta._
import deltas.javac.classes.skeleton._
import deltas.javac.methods.AccessibilityFieldsDelta
import deltas.javac.methods.AccessibilityFieldsDelta.HasAccessibility

object FieldDeclarationDelta extends DeltaWithGrammar
  with HasDeclarationDelta
  with HasConstraintsDelta {

  import deltas.HasNameDelta.Name

  override def description: String = "Enables adding a field declaration without an initializer to a Java class."

  object Shape extends NodeShape
  object Type extends NodeField

  implicit class Field[T <: NodeLike](val node: T) extends HasAccessibility[T] with HasName[T] {
    def _type: T = node(Type).asInstanceOf[T]
  }

  override def dependencies: Set[Contract] = Set(AccessibilityFieldsDelta)

  def neww(_type: Node, name: String) = new Node(Shape, Type -> _type, Name -> name)

  def bind(compilation: Compilation, signature: ClassSignature, field: Node): Unit = {
    val name: String = Field(field).name
    val _type = field._type
    signature.newFieldInfo(name, _type)
  }

  def getFields(javaClass: JavaClass[Node]): Seq[Node] = {
    javaClass.members.filter(member => member.shape == Shape)
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val typeGrammar = find(TypeSkeleton.JavaTypeGrammar)

    val fieldGrammar = find(AccessibilityFieldsDelta.VisibilityField) ~ find(AccessibilityFieldsDelta.Static) ~
      typeGrammar.as(Type) ~~ find(Name) ~< ";" asNode Shape
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
