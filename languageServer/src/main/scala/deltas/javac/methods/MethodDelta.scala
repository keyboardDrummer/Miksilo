package deltas.javac.methods

import core.bigrammar.BiGrammar
import core.bigrammar.grammars.TopBottom
import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.{NodePath, PathRoot}
import core.language.node._
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.objects.Declaration
import core.smarts.scopes.objects.{ConcreteScope, Scope}
import deltas.ConstraintSkeleton
import deltas.bytecode.attributes.CodeAttributeDelta.{CodeAttributesKey, CodeExceptionTableKey, CodeMaxLocalsKey, Instructions}
import deltas.bytecode.attributes.{AttributeNameKey, CodeAttributeDelta}
import deltas.bytecode.constants.Utf8ConstantDelta
import deltas.bytecode.extraConstants.TypeConstant
import deltas.bytecode.simpleBytecode.{InferredMaxStack, InferredStackFrames}
import deltas.bytecode.types.{TypeSkeleton, VoidTypeDelta}
import deltas.bytecode.{ByteCodeMethodInfo, ByteCodeSkeleton}
import deltas.javac.classes.skeleton.JavaClassSkeleton._
import deltas.javac.classes.skeleton._
import deltas.javac.classes.{ClassCompiler, MethodInfo}
import deltas.javac.expressions.ToByteCodeSkeleton
import deltas.javac.methods.AccessibilityFieldsDelta.{HasAccessibility, PrivateVisibility}
import deltas.javac.methods.MethodParameters.MethodParameter
import deltas.javac.types.{MethodType, TypeAbstraction}
import deltas.statement.BlockDelta
import deltas.statement.BlockDelta.BlockStatement



object MethodDelta extends DeltaWithGrammar with WithCompilationState
  with ClassMemberDelta with HasDeclarationDelta with HasConstraintsDelta with HasShape {

  override def description: String = "Enables Java classes to contain methods."

  implicit class Method[T <: NodeLike](node: T) extends HasAccessibility[T](node) {
    def name: String = node(Name).asInstanceOf[String]

    def returnType: T = node(ReturnType).asInstanceOf[T]
    def returnType_=(value: T): Unit = node(ReturnType) = value

    def parameters: Seq[MethodParameter[T]] = NodeWrapper.wrapList(node(Parameters).asInstanceOf[Seq[T]])
    def parameters_=(value: Seq[MethodParameter[T]]): Unit = node(Parameters) = NodeWrapper.unwrapList(value)

    def body: BlockStatement[T] = node(Body).asInstanceOf[T]
  }

  def compile(compilation: Compilation, program: Node): Unit = {
    val classCompiler = JavaClassSkeleton.getClassCompiler(compilation)

    val methods = getMethods(program)
    program(ByteCodeSkeleton.Methods) = methods.map(method => {
      convertMethod(method, classCompiler, compilation)
      method.node
    })
  }

  def bind(compilation: Compilation, signature: ClassSignature, program: Node): Unit = {
    val classCompiler = JavaClassSkeleton.getClassCompiler(compilation)
    val classInfo = classCompiler.currentClassInfo

    val methods = getMethods(program)
    for (method <- methods)
      bindMethod(method)

    def bindMethod(method: Method[Node]): Unit = {
      val methodName: String = MethodDelta.getMethodName(method)
      val parameters = method.parameters
      val parameterTypes = parameters.map(p => getParameterType(PathRoot(p), classCompiler))
      val _type = getMethodType(method)
      val key = MethodClassKey(methodName, parameterTypes.toVector)
      classInfo.methods(key) = MethodInfo(_type, method.isStatic)
    }
  }

  private def getMethodType[T <: NodeLike](method: Method[T]) = {
    val parameterTypes = method.parameters.map(p => p(MethodParameters.Type).asInstanceOf[Node])
    MethodType.construct(method.returnType.asNode, parameterTypes)
  }

  override def dependencies: Set[Contract] = Set(BlockDelta, InferredMaxStack, InferredStackFrames, JavaClassSkeleton,
    AccessibilityFieldsDelta)

  def getParameterType(parameter: MethodParameter[NodePath], classCompiler: ClassCompiler): Node = {
    val result = parameter._type
    result
  }

  def getMethodDescriptor(method: Method[Node], classCompiler: ClassCompiler): Node = {
    TypeConstant.constructor(getMethodType(method))
  }

  def convertMethod(method: Method[Node], classCompiler: ClassCompiler, compilation: Compilation): Unit = {

    method.shape = ByteCodeMethodInfo.Shape
    AccessibilityFieldsDelta.addAccessFlags(method)
    method(ByteCodeMethodInfo.MethodNameIndex) = Utf8ConstantDelta.create(getMethodName(method))
    method.data.remove(Name)
    val methodDescriptorIndex = getMethodDescriptor(method, classCompiler)
    method(ByteCodeMethodInfo.MethodDescriptor) = methodDescriptorIndex
    addCodeAnnotation(PathRoot(method))
    method.data.remove(ReturnType)
    method.data.remove(Parameters)

    def addCodeAnnotation(method: NodePath) {
      setMethodCompiler(method, compilation)
      val statementToInstructions = ToByteCodeSkeleton.getToInstructions(compilation)
      val instructions = statementToInstructions(method.body)
      val exceptionTable = Seq[Node]()
      val codeAttributes = Seq[Node]()
      val methodCompiler = getMethodCompiler(compilation)
      val maxLocalCount: Int = methodCompiler.variablesPerStatement.values.map(pool => pool.localCount).max //TODO move this to a lower level.
      val codeAttribute = new Node(CodeAttributeDelta.CodeKey,
        AttributeNameKey -> CodeAttributeDelta.constantEntry,
        CodeMaxLocalsKey -> maxLocalCount,
        Instructions -> instructions,
        CodeExceptionTableKey -> exceptionTable,
        CodeAttributesKey -> codeAttributes)
      method(ByteCodeMethodInfo.MethodAttributes) = Seq(codeAttribute)
      method.current.data.remove(Body)
    }
  }

  def setMethodCompiler(method: Node, compilation: Compilation) {
    val methodCompiler = MethodCompiler(compilation, method)
    getState(compilation).methodCompiler = methodCompiler
  }

  def getMethodCompiler(compilation: Compilation) = getState(compilation).methodCompiler

  def getMethodName(method: Node) = {
    method(Name).asInstanceOf[String]
  }

  def getMethods[T <: NodeLike](javaClass: JavaClass[T]): Seq[Method[T]] = NodeWrapper.wrapList(javaClass.members.filter(member => member.shape == Shape))


  object ParametersGrammar extends GrammarKey
  object ReturnTypeGrammar extends GrammarKey

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit =  {
    val _grammars = grammars
    import grammars._
    val block = find(BlockDelta.Grammar)

    val parseType = find(TypeSkeleton.JavaTypeGrammar)
    val parseReturnType = create(ReturnTypeGrammar, "void" ~> value(VoidTypeDelta.voidType) | parseType)

    val parseParameter = MethodParameters.getGrammar(_grammars)
    val parseParameters = create(ParametersGrammar, "(" ~> parseParameter.manySeparated(",") ~< ")")


    val typeParametersGrammar: BiGrammar = find(TypeAbstraction.TypeParametersGrammar)

    val methodUnmapped: TopBottom = find(AccessibilityFieldsDelta.VisibilityField) ~
      find(AccessibilityFieldsDelta.Static) ~ typeParametersGrammar.as(TypeParameters) ~
      parseReturnType.as(ReturnType) ~~ identifier.as(Name) ~ parseParameters.as(Parameters) % block.as(Body)
    val methodGrammar = create(MethodGrammar, methodUnmapped.asNode(Shape))

    val memberGrammar = find(JavaClassSkeleton.ClassMemberGrammar)
    memberGrammar.addAlternative(methodGrammar)
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

  def createState = new State()
  class State {
    var methodCompiler: MethodCompiler = _
  }

  object Shape extends NodeShape

  object MethodGrammar extends GrammarKey

  object Body extends NodeField

  object ReturnType extends NodeField

  object Name extends NodeField

  object Parameters extends NodeField

  object TypeParameters extends NodeField

  override def getDeclaration(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Declaration = {
    val method: Method[NodePath] = path
    val parameterTypes = method.parameters.map(p => p(MethodParameters.Type).asInstanceOf[NodePath])
    val returnType = method.returnType
    val methodType = MethodType.getType(compilation, builder, parentScope, parameterTypes, returnType)

    builder.declare(method.name, parentScope, path.getMember(Name), Some(methodType))
  }

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
    getBodyScope(compilation, builder, path, parentScope)
  }

  def getBodyScope(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): ConcreteScope = {
    val method: Method[NodePath] = path
    val bodyScope = builder.newScope(Some(parentScope), "methodBody")
    method.parameters.foreach(parameter => {
      MethodParameters.declare(compilation, builder, parameter, parentScope, bodyScope)
    })
    ConstraintSkeleton.constraints(compilation, builder, method.body, bodyScope)
    bodyScope
  }

  override def shape: NodeShape = Shape
}
