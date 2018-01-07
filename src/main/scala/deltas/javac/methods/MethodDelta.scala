package deltas.javac.methods

import core.bigrammar.BiGrammar
import core.bigrammar.grammars.TopBottom
import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.node._
import core.deltas.path.{Path, PathRoot}
import deltas.bytecode.attributes.CodeAttributeDelta.{CodeAttributesKey, CodeExceptionTableKey, CodeMaxLocalsKey, Instructions}
import deltas.bytecode.attributes.{AttributeNameKey, CodeAttributeDelta}
import deltas.bytecode.constants.Utf8ConstantDelta
import deltas.bytecode.extraConstants.TypeConstant
import deltas.bytecode.simpleBytecode.{InferredMaxStack, InferredStackFrames}
import deltas.bytecode.types.{TypeSkeleton, VoidTypeC}
import deltas.bytecode.{ByteCodeMethodInfo, ByteCodeSkeleton}
import deltas.javac.classes.skeleton.JavaClassSkeleton._
import deltas.javac.classes.skeleton._
import deltas.javac.classes.{ClassCompiler, MethodInfo}
import deltas.javac.methods.AccessibilityFieldsDelta.{HasAccessibility, PrivateVisibility}
import deltas.javac.statements.{BlockDelta, StatementSkeleton}
import deltas.javac.types.{MethodType, TypeAbstraction}

object MethodDelta extends DeltaWithGrammar with WithCompilationState
  with ClassMemberDelta {

  implicit class Method[T <: NodeLike](node: T) extends HasAccessibility[T](node) {
    def returnType: T = node(ReturnTypeKey).asInstanceOf[T]
    def returnType_=(value: T): Unit = node(ReturnTypeKey) = value

    def parameters: Seq[T] = node(MethodParametersKey).asInstanceOf[Seq[T]]
    def parameters_=(value: Seq[T]): Unit = node(MethodParametersKey) = value

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
      val _type = MethodType.construct(method.returnType, parameterTypes)
      val key = MethodClassKey(methodName, parameterTypes.toVector)
      classInfo.methods(key) = MethodInfo(_type, method.isStatic)
    }
  }

  override def dependencies: Set[Contract] = Set(BlockDelta, InferredMaxStack, InferredStackFrames, JavaClassSkeleton,
    AccessibilityFieldsDelta)

  def getParameterType(metaObject: Node, classCompiler: ClassCompiler) = {
    val result = metaObject(ParameterTypeKey).asInstanceOf[Node]
    JavaClassSkeleton.fullyQualify(result, classCompiler)
    result
  }

  def getMethodDescriptor(method: Method[Node], classCompiler: ClassCompiler): Node = {
    val methodType = MethodType.construct(method.returnType, method.parameters.map(p => getParameterType(p, classCompiler)))
    TypeConstant.constructor(methodType)
  }

  def convertMethod(method: Method[Node], classCompiler: ClassCompiler, compilation: Compilation): Unit = {

    method.shape = ByteCodeMethodInfo.MethodInfoKey
    AccessibilityFieldsDelta.addAccessFlags(method)
    method(ByteCodeMethodInfo.MethodNameIndex) = Utf8ConstantDelta.create(getMethodName(method))
    method.data.remove(MethodNameKey)
    val methodDescriptorIndex = getMethodDescriptor(method, classCompiler)
    method(ByteCodeMethodInfo.MethodDescriptor) = methodDescriptorIndex
    addCodeAnnotation(PathRoot(method))
    method.data.remove(ReturnTypeKey)
    method.data.remove(MethodParametersKey)

    def addCodeAnnotation(method: Path) {
      setMethodCompiler(method, compilation)
      val statements = method.body
      method.current.data.remove(Body)
      val statementToInstructions = StatementSkeleton.getToInstructions(compilation)
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
    method(MethodNameKey).asInstanceOf[String]
  }

  def getMethods[T <: NodeLike](javaClass: JavaClass[T]): Seq[Method[T]] = NodeWrapper.wrapList(javaClass.members.filter(member => member.shape == Shape))

  def getParameterName(metaObject: Node) = metaObject(ParameterNameKey).asInstanceOf[String]

  object ParametersGrammar extends GrammarKey
  object ReturnTypeGrammar extends GrammarKey

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit =  {
    import grammars._
    val block = find(BlockDelta.Grammar)

    val parseType = find(TypeSkeleton.JavaTypeGrammar)
    val parseReturnType = create(ReturnTypeGrammar, "void" ~> value(VoidTypeC.voidType) | parseType)

    val parseParameter = parseType.as(ParameterTypeKey) ~~ identifier.as(ParameterNameKey) asNode ParameterKey
    val parseParameters = create(ParametersGrammar, "(" ~> parseParameter.manySeparated(",") ~< ")")


    val typeParametersGrammar: BiGrammar = find(TypeAbstraction.TypeParametersGrammar)

    val methodUnmapped: TopBottom = find(AccessibilityFieldsDelta.VisibilityField) ~
      find(AccessibilityFieldsDelta.Static) ~ typeParametersGrammar.as(TypeParameters) ~
      parseReturnType.as(ReturnTypeKey) ~~ identifier.as(MethodNameKey) ~ parseParameters.as(MethodParametersKey) % block.as(Body)
    val methodGrammar = create(MethodGrammar, methodUnmapped.asNode(Shape))

    val memberGrammar = find(JavaClassSkeleton.ClassMemberGrammar)
    memberGrammar.addOption(methodGrammar)
  }

  def method(name: String, _returnType: Any, _parameters: Seq[Node], _body: Seq[Node],
             static: Boolean = false, visibility: AccessibilityFieldsDelta.Visibility = PrivateVisibility, typeParameters: Seq[Node] = Seq.empty) = {
    new Node(Shape,
      MethodNameKey -> name,
      ReturnTypeKey -> _returnType,
      MethodParametersKey -> _parameters,
      Body -> _body,
      AccessibilityFieldsDelta.Static -> static,
      AccessibilityFieldsDelta.VisibilityField -> visibility,
      TypeParameters -> typeParameters)
  }

  object ParameterKey extends NodeShape
  def parameter(name: String, _type: Any) = {
    new Node(ParameterKey,
      ParameterNameKey -> name,
      ParameterTypeKey -> _type)
  }

  def createState = new State()
  class State {
    var methodCompiler: MethodCompiler = _
  }

  object Shape extends NodeShape

  object MethodGrammar extends GrammarKey

  object Body extends NodeField

  object ParameterNameKey extends NodeField

  object ReturnTypeKey extends NodeField

  object MethodNameKey extends NodeField

  object MethodParametersKey extends NodeField

  object TypeParameters extends NodeField

  object ParameterTypeKey extends NodeField

  override def description: String = "Enables Java classes to contain methods."
}
