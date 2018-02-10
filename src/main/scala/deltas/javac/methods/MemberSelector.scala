package deltas.javac.methods

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.node._
import core.deltas.path.Path
import core.language.Language
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import deltas.javac.classes._
import deltas.javac.classes.skeleton.{ClassSignature, JavaClassSkeleton}
import deltas.javac.expressions.ExpressionSkeleton

object MemberSelector extends DeltaWithGrammar with WithLanguageRegistry {
  def getScope(compilation: Compilation, builder: ConstraintBuilder, target: Path, parentScope: Scope): Scope = {
    getRegistry(compilation).getScope(target.shape)(compilation,builder,target, parentScope)
  }

  def getSelectorTarget[T <: NodeLike](selector: T): T = selector(Target).asInstanceOf[T]

  def getSelectorMember(selector: Node): String = selector(Member).asInstanceOf[String]

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val expression = find(ExpressionSkeleton.ExpressionGrammar)
    val selection = (expression.as(Target) ~< ".") ~ identifier.as(Member) asNode Shape
    create(SelectGrammar, selection)
  }

  object SelectGrammar extends GrammarKey

  trait MethodContainerExpressionShape extends NodeShape {
    def getScope(compilation: Compilation, builder: ConstraintBuilder, expression: Path, scope: Scope): Scope
  }

  object Shape extends NodeShape

  object Target  extends NodeField

  object Member extends NodeField

  def selector(_object: Any, member: Any): Node = selector(_object.asInstanceOf[Node], member.asInstanceOf[String])

  def selector(_object: Node, member: String): Node = {
    new Node(Shape,
      Target -> _object,
      Member -> member)
  }

  def getClassOrObjectReference(selector: Path, compiler: ClassCompiler): ClassOrObjectReference = {
    val obj = getSelectorTarget(selector)
    getReferenceKind(compiler, obj).asInstanceOf[ClassOrObjectReference]
  }

  def getReferenceKind(classCompiler: ClassCompiler, expression: Path): ReferenceKind = {
    val getReferenceKindOption = MemberSelector.getReferenceKindRegistry(classCompiler.compilation).get(expression.shape)
    getReferenceKindOption.fold[ReferenceKind]({
      getReferenceKindFromExpressionType(classCompiler, expression)
    })(implementation => implementation(classCompiler.compilation, expression))
  }

  def getReferenceKindFromExpressionType(classCompiler: ClassCompiler, expression: Path): ClassOrObjectReference = {
    val classInfo: ClassSignature = classCompiler.findClass(ExpressionSkeleton.getType(classCompiler.compilation)(expression))
    ClassOrObjectReference(classInfo, wasClass = false)
  }

  def getReferenceKindRegistry(language: Language) = getRegistry(language).referenceKindRegistry
  class Registry {
    val referenceKindRegistry = new ShapeRegistry[(Compilation, Path) => ReferenceKind]()
    val getScope = new ShapeRegistry[(Compilation, ConstraintBuilder, Path, Scope) => Scope]()
  }

  override def createRegistry = new Registry()

  override def description: String = "Defines the selector grammar <expression>.<identifier>"

  override def dependencies: Set[Contract] = Set(JavaClassSkeleton)
}
