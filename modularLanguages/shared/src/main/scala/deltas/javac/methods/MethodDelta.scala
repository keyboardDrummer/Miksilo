package deltas.javac.methods

import core.bigrammar.BiGrammar
import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.{NodePath, PathRoot}
import core.language.node._
import core.language.{Compilation, CompilationState, Language}
import core.smarts.ConstraintBuilder
import core.smarts.objects.Declaration
import core.smarts.scopes.objects.{ConcreteScope, Scope}
import deltas.ConstraintSkeleton
import deltas.bytecode.extraConstants.TypeConstant
import deltas.bytecode.types.{TypeSkeleton, VoidTypeDelta}
import deltas.javac.classes.{ClassCompiler, MethodInfo}
import deltas.javac.classes.skeleton.JavaClassDelta.JavaClass
import deltas.javac.classes.skeleton.{ClassSignature, HasConstraintsDelta, HasDeclarationDelta, JavaClassDelta, MethodClassKey}
import deltas.javac.methods.AccessibilityFieldsDelta.{HasAccessibility, PrivateVisibility}
import deltas.javac.methods.MethodParameters.MethodParameter
import deltas.javac.types.{MethodTypeDelta, TypeAbstraction}
import deltas.statement.{BlockDelta, LabelStatementDelta}
import deltas.statement.BlockDelta.BlockStatement

object MethodDelta extends DeltaWithGrammar
  with HasDeclarationDelta with HasConstraintsDelta with HasShape {

  import deltas.HasNameDelta._

  override def description: String = "Enables Java classes to contain methods."

  implicit class Method[T <: NodeLike](val node: T) extends HasAccessibility[T] with HasName[T] {

    def returnType: T = node(ReturnType).asInstanceOf[T]
    def returnType_=(value: T): Unit = node(ReturnType) = value

    def parameters: Seq[MethodParameter[T]] = NodeWrapper.wrapList(node(Parameters).asInstanceOf[Seq[T]])
    def parameters_=(value: Seq[MethodParameter[T]]): Unit = node(Parameters) = NodeWrapper.unwrapList(value)

    def body: BlockStatement[T] = node(Body).asInstanceOf[T]
  }

  def bind(compilation: Compilation, signature: ClassSignature, method: Method[Node]): Unit = {
    val classCompiler = JavaClassDelta.getClassCompiler(compilation)
    val classInfo = classCompiler.currentClassInfo

    val methodName: String = MethodDelta.getMethodName(method)
    val parameters = method.parameters
    val parameterTypes = parameters.map(p => getParameterType(PathRoot(p), classCompiler))
    val _type = getMethodType(method)
    val key = MethodClassKey(methodName, parameterTypes.toVector)
    classInfo.methods(key) = MethodInfo(_type, method.isStatic)
  }

  def getMethodType[T <: NodeLike](method: Method[T]) = {
    val parameterTypes = method.parameters.map(p => p(MethodParameters.Type).asInstanceOf[T].asNode)
    MethodTypeDelta.neww(method.returnType.asNode, parameterTypes)
  }

  override def dependencies: Set[Contract] = Set(BlockDelta, AccessibilityFieldsDelta)

  def getParameterType(parameter: MethodParameter[NodePath], classCompiler: ClassCompiler): Node = {
    val result = parameter._type
    result
  }

  def getMethodDescriptor(method: Method[Node], classCompiler: ClassCompiler): Node = {
    TypeConstant.constructor(getMethodType(method))
  }


  def setMethodCompiler(method: Node, compilation: Compilation): Unit = {
    state(compilation) = MethodCompiler(compilation, method)
  }

  def getMethodCompiler(compilation: Compilation) = state(compilation)

  def getMethodName(method: Node) = {
    method(Name).asInstanceOf[String]
  }

  def getMethods[T <: NodeLike](javaClass: JavaClass[T]): Seq[Method[T]] =
    NodeWrapper.wrapList(javaClass.members.filter(member => member.shape == Shape))

  object ReturnTypeGrammar extends GrammarKey

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit =  {
    val _grammars = grammars
    import grammars._
    val block = find(BlockDelta.BlockGrammar)

    val parseType = find(TypeSkeleton.JavaTypeGrammar)
    val parseReturnType = create(ReturnTypeGrammar, parseType)

    val parseParameter = MethodParameters.getGrammar(_grammars)
    val parseParameters = create(Parameters, "(" ~> parseParameter.manySeparated(",") ~< ")")

    val typeParametersGrammar: BiGrammar = find(TypeAbstraction.TypeParametersGrammar)

    val methodUnmapped: BiGrammar = find(AccessibilityFieldsDelta.VisibilityField) ~
      find(AccessibilityFieldsDelta.Static) ~ typeParametersGrammar.as(TypeParameters) ~
      parseReturnType.as(ReturnType) ~~ find(Name) ~ parseParameters.as(Parameters) % block.as(Body)
    create(Shape, methodUnmapped.asNode(Shape))
  }

  def neww(name: String, _returnType: Any, _parameters: Seq[Node], _body: Node,
           static: Boolean = false,
           visibility: AccessibilityFieldsDelta.Visibility = PrivateVisibility,
           typeParameters: Seq[Node] = Seq.empty): Node = {
    new Node(Shape,
      Name -> name,
      ReturnType -> _returnType,
      Parameters -> _parameters,
      Body -> _body,
      AccessibilityFieldsDelta.Static -> static,
      AccessibilityFieldsDelta.VisibilityField -> visibility,
      TypeParameters -> typeParameters)
  }

  val state = new CompilationState[MethodCompiler](null)

  object Shape extends NodeShape

  object Body extends NodeField

  object ReturnType extends NodeField

  object Parameters extends NodeField

  object TypeParameters extends NodeField

  override def getDeclaration(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Declaration = {
    val method: Method[NodePath] = path
    val parameterTypes = method.parameters.map(p => p(MethodParameters.Type).asInstanceOf[NodePath])
    val returnType = method.returnType
    val methodType = MethodTypeDelta.getType(compilation, builder, parentScope, parameterTypes, returnType)

    builder.declare(method.name, parentScope, path.getField(Name), Some(methodType))
  }

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
    getBodyScope(compilation, builder, path, parentScope)
  }

  def getBodyScope(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): ConcreteScope = {
    val method: Method[NodePath] = path
    val bodyScope = builder.newScope(parentScope, "methodBody")
    method.parameters.foreach(parameter => {
      MethodParameters.declare(compilation, builder, parameter, parentScope, bodyScope)
    })
    ConstraintSkeleton.constraints(compilation, builder, method.body, bodyScope)
    bodyScope
  }

  override def shape: NodeShape = Shape

  override def inject(language: Language): Unit = {
    LabelStatementDelta.isLabelScope.add(language, Shape, ())
    super.inject(language)
  }
}
