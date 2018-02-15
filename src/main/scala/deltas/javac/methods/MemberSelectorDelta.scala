package deltas.javac.methods

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.language.node._
import core.deltas.path.{ChildPath, NodePath}
import core.language.{Compilation, Language, SourceElement}
import core.smarts.{Constraint, ConstraintBuilder, ConstraintSolver}
import core.smarts.objects.{Declaration, DeclarationVariable, NamedDeclaration}
import core.smarts.scopes.objects.Scope
import core.smarts.types.DeclarationHasType
import deltas.javac.classes._
import deltas.javac.classes.skeleton.{ClassSignature, JavaClassSkeleton}
import deltas.javac.expressions.ExpressionSkeleton

object MemberSelectorDelta extends DeltaWithGrammar with WithLanguageRegistry with ResolvesToDeclaration {

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

  override def getResolvedDeclaration(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, scope: Scope): Declaration = {
    val memberSelector: MemberSelector[NodePath] = expression
    val target = memberSelector.target
    val targetDeclaration = resolvedToDeclaration.get(compilation, target.shape).getResolvedDeclaration(compilation, builder, target, scope)
    val result = builder.declarationVariable()
    builder.add(ResolveSelectorConstraint(targetDeclaration, memberSelector.member, expression.getLocation(Member), result) )
    result
    //De out reference blijkt in de externalSystemClass scope te leven, en kan daarom out niet vinden, want die zit in de internalSystemClass scope.
    //Waarom resolved System in System.out naar de package en niet de class?
  }

  case class ResolveSelectorConstraint(var targetDeclaration: Declaration, member: String, memberSource: SourceElement,
                                       var resolvesTo: Declaration) extends Constraint {
    override def apply(solver: ConstraintSolver): Boolean = {
      targetDeclaration match {
        case e:NamedDeclaration =>
          val path = e.origin.asInstanceOf[NodePath]
          val node = path.current
          val targetScope = node.shape match {
            case JavaClassSkeleton.Shape => //TODO allow referencing packages.
              solver.builder.getDeclaredScope(targetDeclaration)
            case _ =>
              val objectType = solver.builder.getType(targetDeclaration)
              val objectDeclaration = solver.builder.getDeclarationOfType(objectType)
              solver.builder.getDeclaredScope(objectDeclaration)
          }
          solver.builder.reference(member, memberSource, targetScope, resolvesTo)
          true
        case _ => false
      }
    }

    override def instantiateDeclaration(variable: DeclarationVariable, instance: Declaration): Unit = {
      if (targetDeclaration == variable)
        targetDeclaration = instance
      if (resolvesTo == variable)
        resolvesTo = instance
      super.instantiateDeclaration(variable, instance)
    }
  }

  override def shape: NodeShape = Shape

  object SelectGrammar extends GrammarKey

  def getResolvedToDeclaration(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, scope: Scope) : Declaration = {
    resolvedToDeclaration.get(compilation, expression.shape).getResolvedDeclaration(compilation,builder,expression,scope)
  }

  val resolvedToDeclaration: ShapeProperty[ResolvesToDeclaration] = new ShapeProperty[ResolvesToDeclaration]

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
