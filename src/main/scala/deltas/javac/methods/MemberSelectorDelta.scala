package deltas.javac.methods

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node._
import core.language.{Compilation, Language}
import core.smarts.objects.{Declaration, DeclarationVariable, NamedDeclaration}
import core.smarts.scopes.objects.{Scope, ScopeVariable}
import core.smarts.{Constraint, ConstraintBuilder, ConstraintSolver}
import deltas.javac.classes._
import deltas.javac.classes.skeleton.{ClassSignature, JavaClassSkeleton}
import deltas.javac.expressions.ExpressionSkeleton

object MemberSelectorDelta extends DeltaWithGrammar with WithLanguageRegistry with IsNamespaceOrObjectExpression {

  implicit class MemberSelector[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def member: String = node(Member).asInstanceOf[String]
    def target: T = node(Target).asInstanceOf[T]
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val expression = find(ExpressionSkeleton.ExpressionGrammar)
    val selection = (expression.as(Target) ~< ".") ~ identifier.as(Member) asNode Shape
    create(SelectGrammar, selection)
  }

  override def getScopeDeclaration(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, scope: Scope): Declaration = {
    val memberSelector: MemberSelector[NodePath] = expression
    val target = memberSelector.target
    val targetScope = NamespaceOrObjectExpression.getScope(compilation, builder, target, scope)
    val namespaceOrObjectVariableDeclaration =
      builder.resolve(memberSelector.member, expression.getLocation(Member), targetScope)
    val result = builder.declarationVariable()
    builder.add(ResolveNamespaceOrObjectVariableAmbiguity(namespaceOrObjectVariableDeclaration, result))
    result
  }

  override def shape: NodeShape = Shape

  object SelectGrammar extends GrammarKey

  object Shape extends NodeShape

  object Target  extends NodeField

  object Member extends NodeField

  def selector(_object: Any, member: Any): Node = selector(_object.asInstanceOf[Node], member.asInstanceOf[String])

  def selector(_object: Node, member: String): Node = {
    new Node(Shape,
      Target -> _object,
      Member -> member)
  }

  def getClassOrObjectReference(selector: MemberSelector[NodePath], compiler: ClassCompiler): ClassOrObjectReference = {
    val obj = selector.target
    getReferenceKind(compiler, obj).asInstanceOf[ClassOrObjectReference]
  }

  def getReferenceKind(classCompiler: ClassCompiler, expression: NodePath): ReferenceKind = {
    val getReferenceKindOption = MemberSelectorDelta.getReferenceKindRegistry(classCompiler.compilation).get(expression.shape)
    getReferenceKindOption.fold[ReferenceKind]({
      getReferenceKindFromExpressionType(classCompiler, expression)
    })(implementation => implementation(classCompiler.compilation, expression))
  }

  def getReferenceKindFromExpressionType(classCompiler: ClassCompiler, expression: NodePath): ClassOrObjectReference = {
    val classInfo: ClassSignature = classCompiler.findClass(ExpressionSkeleton.getType(classCompiler.compilation)(expression))
    ClassOrObjectReference(classInfo, wasClass = false)
  }

  def getReferenceKindRegistry(language: Language) = getRegistry(language).referenceKindRegistry
  class Registry {
    val referenceKindRegistry = new ShapeRegistry[(Compilation, NodePath) => ReferenceKind]()
    val getScope = new ShapeRegistry[(Compilation, ConstraintBuilder, NodePath, Scope) => Scope]()
  }

  override def createRegistry = new Registry()

  override def description: String = "Defines the selector grammar <expression>.<identifier>"

  override def dependencies: Set[Contract] = Set(JavaClassSkeleton)
}
