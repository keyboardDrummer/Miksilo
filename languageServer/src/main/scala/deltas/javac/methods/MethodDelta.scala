package deltas.javac.methods

import core.bigrammar.BiGrammar
import core.bigrammar.grammars.TopBottom
import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.{ChildPath, NodePath, PathRoot}
import core.language.node._
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.objects.Declaration
import core.smarts.scopes.objects.Scope
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
import deltas.javac.methods.AccessibilityFieldsDelta.{HasAccessibility, PrivateVisibility}
import deltas.javac.statements.ByteCodeStatementSkeleton
import deltas.javac.types.{MethodType, TypeAbstraction}
import deltas.statement.BlockDelta

object MethodDelta extends DeltaWithGrammar with WithCompilationState
  with ClassMemberDelta with HasDeclaration with HasConstraints with HasShape {

  override def description: String = "Enables Java classes to contain methods."

  implicit class Method[T <: NodeLike](node: T) extends HasAccessibility[T](node) {
    def name: String = node(Name).asInstanceOf[String]

    def returnType: T = node(ReturnType).asInstanceOf[T]
    def returnType_=(value: T): Unit = node(ReturnType) = value

    def parameters: Seq[MethodParameter[T]] = NodeWrapper.wrapList(node(Parameters).asInstanceOf[Seq[T]])
    def parameters_=(value: Seq[MethodParameter[T]]): Unit = node(Parameters) = NodeWrapper.unwrapList(value)

    def body: Seq[T] = node(Body).asInstanceOf[Seq[T]]
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

    def bindMethod(method: Method[Node]) = {
      val methodName: String = MethodDelta.getMethodName(method)
      val parameters = method.parameters
      val parameterTypes = parameters.map(p => getParameterType(p, classCompiler))
      val _type = getMethodType(method)
      val key = MethodClassKey(methodName, parameterTypes.toVector)
      classInfo.methods(key) = MethodInfo(_type, method.isStatic)
    }
  }

  private def getMethodType[T <: NodeLike](method: Method[T]) = {
    val parameterTypes = method.parameters.map(p => p(ParameterType).asInstanceOf[Node])
    MethodType.construct(method.returnType.asNode, parameterTypes)
  }

  override def dependencies: Set[Contract] = Set(BlockDelta, InferredMaxStack, InferredStackFrames, JavaClassSkeleton,
    AccessibilityFieldsDelta)

  def getParameterType(metaObject: Node, classCompiler: ClassCompiler): Node = {
    val result = PathRoot(metaObject)(ParameterType).asInstanceOf[NodePath]
    JavaClassSkeleton.fullyQualify(result, classCompiler)
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
      val statements = method.body
      method.current.data.remove(Body)
      val statementToInstructions = ByteCodeStatementSkeleton.getToInstructions(compilation)
      val instructions = statements.flatMap(statement => statementToInstructions(statement))
      val exceptionTable = Seq[Node]()
      val codeAttributes = Seq[Node]()
      val maxLocalCount: Int = getMethodCompiler(compilation).variablesPerStatement.values.map(pool => pool.localCount).max //TODO move this to a lower level.
      val codeAttribute = new Node(CodeAttributeDelta.CodeKey,
        AttributeNameKey -> CodeAttributeDelta.constantEntry,
        CodeMaxLocalsKey -> maxLocalCount,
        Instructions -> instructions,
        CodeExceptionTableKey -> exceptionTable,
        CodeAttributesKey -> codeAttributes)
      method(ByteCodeMethodInfo.MethodAttributes) = Seq(codeAttribute)
    }
  }

  def setMethodCompiler(method: Node, compilation: Compilation) {
    val methodCompiler = new MethodCompiler(compilation, method)
    getState(compilation).methodCompiler = methodCompiler
  }

  def getMethodCompiler(compilation: Compilation) = getState(compilation).methodCompiler

  def getMethodName(method: Node) = {
    method(Name).asInstanceOf[String]
  }

  def getMethods[T <: NodeLike](javaClass: JavaClass[T]): Seq[Method[T]] = NodeWrapper.wrapList(javaClass.members.filter(member => member.shape == Shape))

  def getParameterName(metaObject: Node) = metaObject(ParameterName).asInstanceOf[String]

  object ParametersGrammar extends GrammarKey
  object ReturnTypeGrammar extends GrammarKey

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit =  {
    import grammars._
    val block = find(BlockDelta.Grammar)

    val parseType = find(TypeSkeleton.JavaTypeGrammar)
    val parseReturnType = create(ReturnTypeGrammar, "void" ~> value(VoidTypeDelta.voidType) | parseType)

    val parseParameter = parseType.as(ParameterType) ~~ identifier.as(ParameterName) asNode ParameterKey
    val parseParameters = create(ParametersGrammar, "(" ~> parseParameter.manySeparated(",") ~< ")")


    val typeParametersGrammar: BiGrammar = find(TypeAbstraction.TypeParametersGrammar)

    val methodUnmapped: TopBottom = find(AccessibilityFieldsDelta.VisibilityField) ~
      find(AccessibilityFieldsDelta.Static) ~ typeParametersGrammar.as(TypeParameters) ~
      parseReturnType.as(ReturnType) ~~ identifier.as(Name) ~ parseParameters.as(Parameters) % block.as(Body)
    val methodGrammar = create(MethodGrammar, methodUnmapped.asNode(Shape))

    val memberGrammar = find(JavaClassSkeleton.ClassMemberGrammar)
    memberGrammar.addAlternative(methodGrammar)
  }

  def method(name: String, _returnType: Any, _parameters: Seq[Node], _body: Seq[Node],
             static: Boolean = false, visibility: AccessibilityFieldsDelta.Visibility = PrivateVisibility, typeParameters: Seq[Node] = Seq.empty) = {
    new Node(Shape,
      Name -> name,
      ReturnType -> _returnType,
      Parameters -> _parameters,
      Body -> _body,
      AccessibilityFieldsDelta.Static -> static,
      AccessibilityFieldsDelta.VisibilityField -> visibility,
      TypeParameters -> typeParameters)
  }

  object ParameterKey extends NodeShape
  def parameter(name: String, _type: Any) = {
    new Node(ParameterKey,
      ParameterName -> name,
      ParameterType -> _type)
  }

  def createState = new State()
  class State {
    var methodCompiler: MethodCompiler = _
  }

  object Shape extends NodeShape

  object MethodGrammar extends GrammarKey

  object Body extends NodeField

  object ParameterName extends NodeField

  object ReturnType extends NodeField

  object Name extends NodeField

  object Parameters extends NodeField

  object TypeParameters extends NodeField

  object ParameterType extends NodeField

  implicit class MethodParameter[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def _type: T = node(ParameterType).asInstanceOf[T]
    def _type_=(value: T): Unit = node(ParameterType) = value

    def name: Any = node(ParameterName)
    def name_=(value: Any): Unit = node(ParameterName) = value
  }

  override def inject(language: Language): Unit = {
    super.inject(language)
    JavaClassSkeleton.hasDeclarations.add(language, Shape, this)
    JavaClassSkeleton.hasConstraints.add(language, this)
  }

  override def getDeclaration(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Declaration = {
    val method: Method[NodePath] = path
    val parameterTypes = method.parameters.map(p => p(ParameterType).asInstanceOf[NodePath])
    val returnType = method.returnType
    val methodType = MethodType.getType(compilation, builder, parentScope, parameterTypes, returnType)

    builder.declare(method.name, parentScope, path.getLocation(Name), Some(methodType))
  }

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
    getBodyScope(compilation, builder, path, parentScope)
  }

  def getBodyScope(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope) = {
    val method: Method[NodePath] = path
    val bodyScope = builder.newScope(Some(parentScope), "methodBody")
    method.parameters.foreach(parameter => {
      val parameterType = TypeSkeleton.getType(compilation, builder, parameter._type, parentScope)
      val name = parameter(ParameterName).asInstanceOf[String]
      builder.declare(name, bodyScope, parameter.getLocation(ParameterName), Some(parameterType))
    })
    BlockDelta.collectConstraints(compilation, builder, method.body.asInstanceOf[Seq[ChildPath]], bodyScope)
    bodyScope
  }

  override def shape: NodeShape = Shape
}
